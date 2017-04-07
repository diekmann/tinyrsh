extern crate tinyrsh;

use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};
use std::io;
use std::process;
use std::os::unix::io::{RawFd, AsRawFd};
use tinyrsh::select::FdSet;
use tinyrsh::child::PersistentChild;
use tinyrsh::fdstore::{FdStore, OnceAction, OnceAccept, OnceRead};


fn greet_client(fdstore: &FdStore, mut stream: &TcpStream) {
   assert_eq!(stream.write(b"hello\n").expect("greet client failed"), 6);
   stream.write(b"Your fellow peers:");
   let mut v = vec![];
   for other in fdstore.clients.keys() {
       v.push(format!("fd {}", other));
   }
   stream.write(v.join(", ").as_bytes());
   stream.write(b"\n");
   for mut client in fdstore.clients.values() {
       client.write(b"new client connected\n");
   }
}

fn read_child<T: Read>(readt: &mut T) {
    let mut buf = [0; 10];
    let n = readt.read(&mut buf).expect("read_child");
    println!("({} bytes) `{}'", n, String::from_utf8_lossy(&buf[..n]));
}

fn forward_to_remotes<T: Read, S>(child_out: &mut T, clients: &HashMap<S, TcpStream>) -> bool 
    where S : std::cmp::Eq, S : std::hash::Hash
{
    let mut buf = [0; 10];
    let n = child_out.read(&mut buf).expect("read_child");
    let child_eof = (n == 0);
    if clients.is_empty() {
        println!("Warning: child output but no remote connected");
        println!("{:?} ({} bytes) `{}'", buf, n, String::from_utf8_lossy(&buf[..n]));
    }
    for mut remote in clients.values(){
        //println!("forwarding to {}", remote.peer_addr().unwrap());
        let written = remote.write(&buf[..n]).expect("remote write");
        assert_eq!(n, written);
    }
    child_eof
}


fn copy_to<T: Read, U: Write>(from: &mut T, to: &mut U) -> bool {
    let mut buf = [0; 10];
    let n = from.read(&mut buf).expect("copy_to read");
    //println!("copy_to read {:?} ({} bytes) `{}'", buf, n, String::from_utf8_lossy(&buf[..n]));
    let written = to.write(&buf[..n]).expect("copy_to write");
    to.flush();
    assert_eq!(n, written);

    n != 0
}



fn main() {
    println!("Hello, world!");

    let mut child = PersistentChild::new("../py-ptysh/ptysh.py");


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
            let child_eof = (n == 0);
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
       let active_vec = fdstore.select(&srvacceptfun, &clientfun, &childstdoutfun);
       for active in active_vec {
       match active {
          OnceAction::Other(fd) => {
          if fdstore.child.is_stderr(fd) {
              println!("child fd {} (stderr) got active", fd);
              read_child(fdstore.child.stderr_as_mut());
          } else {
              assert!(false, "unknown fd");
          }
          }//Other
    } //match
    } //for
    } //loop
}
