extern crate tinyrsh;

use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::io::{Write};
use std::io;
use std::process;
use std::os::unix::io::{RawFd};
use tinyrsh::child::PersistentChild;
use tinyrsh::fdstore::{FdStore, OnceAccept, OnceRead, Clients};


#[derive(Debug)]
struct AuxData {
    total_cons: u64,
    rbytes: u64,
    wbytes: u64,
}

//print only safe ascii characters for debugging. In particular don't print shell escapes.
fn safe_ascii_debug(v: &[u8]) -> String {
    let mut s = String::new();
    for &c in v.iter() {
        if c >= 0x20u8 && c <= 0x7Eu8 && c != 0x5Cu8 {
            s.push(c as char);
        } else {
            s.push('X')
        }
    }
    s
}

fn main() {
    println!("Hello, world!");

    let mut aux = AuxData{ total_cons: 0, rbytes:0, wbytes:0 };

    let child = PersistentChild::new("../py-ptysh/ptysh.py");

    assert!(!child.has_terminated());

    // TODO linebuffered? how is child output buffered at all?
    let listener = TcpListener::bind("127.0.0.1:6699").unwrap();

    let mut fdstore = FdStore::new(listener, child, aux);
    
    
   println!("select-looping");
   loop {
       let srvacceptfun = |  a: OnceAccept, clients: &Clients, aux: &mut AuxData | -> io::Result<TcpStream> {
              println!("Accepting new connection");
              aux.total_cons += 1;
              println!("{:?}", aux);
              a.do_accept().map( | (mut stream, addr) | {
                      println!("new client: {:?}", addr);
                      assert_eq!(stream.write(b"hello\n").expect("greet client failed"), 6);
                      stream.write_all(b"Your fellow peers:").expect("client write");
                      let mut v = vec![];
                      for other in clients.values() {
                          v.push(format!("ip {}", other.peer_addr().expect("peer addr")));
                      }
                      stream.write_all(v.join(", ").as_bytes()).expect("client write");
                      stream.write(b"\n").expect("client write");
                      for mut client in clients.values() {
                          client.write_all(b"new client connected\n").expect("client write");
                      }
                      stream
              })
           };
       let clientfun = | client: OnceRead<TcpStream>, to: &mut process::ChildStdin, aux: &mut AuxData | -> bool {
            let mut buf = [0; 1024];
            let n = client.do_read(&mut buf).expect("copy_to read");
            let written = to.write(&buf[..n]).expect("copy_to write");
            to.flush();
            assert_eq!(n, written);
            aux.rbytes += n as u64;
            n != 0
           };
       let childstdoutfun = | stdout: OnceRead<process::ChildStdout>, clients: &Clients, aux: &mut AuxData | -> bool {
            println!("child (stdout) got active");
            let mut buf = [0; 1024];
            let n = stdout.do_read(&mut buf).expect("read_child");
            let child_eof = n == 0;
            if clients.is_empty() {
                println!("Warning: child output but no remote connected");
                println!("({} bytes) `{}'", n, safe_ascii_debug(&buf[..n]));
            }
            for mut remote in clients.values(){
                //println!("forwarding to {}", remote.peer_addr().unwrap());
                let written = remote.write(&buf[..n]).expect("remote write");
                assert_eq!(n, written);
                aux.wbytes += written as u64;
            }
            !child_eof
           };
       let childstderrfun = | stderr: OnceRead<process::ChildStderr>, clients: &Clients, aux: &mut AuxData | -> bool {
            println!("child (stderr) got active");
            let mut buf = [0; 1024];
            let n = stderr.do_read(&mut buf).expect("read_child");
            println!("({} bytes) `{}'", n, safe_ascii_debug(&buf[..n]));
            n != 0
           };

       //do all the stuff
       fdstore.select(&srvacceptfun, &clientfun, &childstdoutfun, &childstderrfun);
   } //loop
}
