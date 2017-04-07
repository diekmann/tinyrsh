extern crate tinyrsh;

use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::io::{Write};
use std::io;
use std::process;
use std::os::unix::io::{RawFd};
use tinyrsh::child::PersistentChild;
use tinyrsh::fdstore::{FdStore, OnceAccept, OnceRead};


fn greet_client(fdstore: &FdStore, mut stream: &TcpStream) {
   assert_eq!(stream.write(b"hello\n").expect("greet client failed"), 6);
   stream.write_all(b"Your fellow peers:").expect("client write");
   let mut v = vec![];
   for other in fdstore.clients.keys() {
       v.push(format!("fd {}", other));
   }
   stream.write_all(v.join(", ").as_bytes()).expect("client write");
   stream.write(b"\n").expect("client write");
   for mut client in fdstore.clients.values() {
       client.write_all(b"new client connected\n").expect("client write");
   }
}


fn main() {
    println!("Hello, world!");

    let child = PersistentChild::new("../../py-ptysh/ptysh.py");
    // TODO linebuffered? how is child output buffered at all?
    let listener = TcpListener::bind("127.0.0.1:6699").unwrap();

    let mut fdstore = FdStore::new(listener, child);
    
    
   println!("select-looping");
   loop {
       //let readfds = fdstore.all_fdset.readfds_select();
       //for fd in readfds {
       let srvacceptfun = |  a: OnceAccept, allfd: &FdStore | -> io::Result<TcpStream> {
              println!("Accepting new connection");
              a.do_accept().map( | (mut stream, addr) | {
                      println!("new client: {:?}", addr);
                      greet_client(allfd, &mut stream);
                      stream
              })
           };
       let clientfun = | client: OnceRead<TcpStream>, to: &mut process::ChildStdin | -> bool {
            let mut buf = [0; 10];
            let n = client.do_read(&mut buf).expect("copy_to read");
            let written = to.write(&buf[..n]).expect("copy_to write");
            to.flush();
            assert_eq!(n, written);
            n != 0
           };
       let childstdoutfun = | stdout: OnceRead<process::ChildStdout>, clients: &HashMap<RawFd, TcpStream> | -> () {
            println!("child (stdout) got active");
            let mut buf = [0; 10];
            let n = stdout.do_read(&mut buf).expect("read_child");
            let child_eof = n == 0;
            if clients.is_empty() {
                println!("Warning: child output but no remote connected");
                println!("{:?} ({} bytes) `{}'", buf, n, String::from_utf8_lossy(&buf[..n]));
            }
            for mut remote in clients.values(){
                //println!("forwarding to {}", remote.peer_addr().unwrap());
                let written = remote.write(&buf[..n]).expect("remote write");
                assert_eq!(n, written);
            }
            if child_eof {
                println!("!!!!!!! child exited. pipe will break now");
                //println!("exit code: {}", child.wait().expect("child unexpected state"));
                //ahh, ownership!
            }
            assert!(!child_eof, "child exited, unhandled!");
           };
       let childstderrfun = | stderr: OnceRead<process::ChildStderr>, clients: &HashMap<RawFd, TcpStream> | -> () {
            println!("child (stderr) got active");
            let mut buf = [0; 10];
            let n = stderr.do_read(&mut buf).expect("read_child");
            println!("({} bytes) `{}'", n, String::from_utf8_lossy(&buf[..n]));
           };

       //do all the stuff
       fdstore.select(&srvacceptfun, &clientfun, &childstdoutfun, &childstderrfun);
   } //loop
}
