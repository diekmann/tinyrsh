extern crate tinyrsh;

use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};
use std::io;
use std::os::unix::io::{RawFd, AsRawFd};
use tinyrsh::select::FdSet;
use tinyrsh::child::PersistentChild;
use tinyrsh::fdstore::{FdStore, OnceAction, OnceAccept};


fn greet_client(mut stream: &TcpStream) {
    let remote = stream.peer_addr().unwrap();
    println!("connected: {}", remote);

    match stream.write(b"hello\n") {
        Err(e) => println!("{}", e),
        Ok(n) => println!("wrote {} bytes", n),
    };
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

    let mut fdstore = FdStore::new(listener);
    
    for fd in [child.child.stdout.as_ref().unwrap().as_raw_fd(), child.child.stderr.as_ref().unwrap().as_raw_fd()].iter() {
        println!("inserting child fd {}", fd);
        fdstore.all_fdset.insert(*fd);
    };

    
   println!("select-looping");
   loop {
       //let readfds = fdstore.all_fdset.readfds_select();
       //for fd in readfds {
       let srvacceptfun = | allfd: &FdStore, a: OnceAccept | -> io::Result<TcpStream> {
              println!("Accepting new connection");
              match a.do_accept() {
                  Err(e) => { println!("couldn't get client: {:?}", e);
                              Err(e)
                  }
                  Ok((mut stream, addr)) => {
                      println!("new client: {:?}", addr);
                      greet_client(&mut stream);
                      Ok(stream)
                  }
              }
            };
       let active_vec = fdstore.select(&srvacceptfun);
       for active in active_vec {
       match active {
          OnceAction::Other(fd) => {
          if child.is_stdout(fd) {
              println!("child fd {} (stdout) got active", fd);
              let child_eof = forward_to_remotes(child.stdout_as_mut(), &fdstore.clients);
              if child_eof {
                  println!("!!!!!!! child exited. pipe will break now");
                  //println!("exit code: {}", child.wait().expect("child unexpected state"));
                  //ahh, ownership!
              }
              assert!(!child_eof, "child exited, unhandled!");
          } else if child.is_stderr(fd) {
              println!("child fd {} (stderr) got active", fd);
              read_child(child.stderr_as_mut());
          } else {
              assert!(fdstore.clients.contains_key(&fd));
              let cont = {
                  //let ref stream = clients[&i];
                  let stream: &mut TcpStream = fdstore.clients.get_mut(&fd).unwrap();
                  //read_client(stream)
                  copy_to(stream, child.stdin_as_mut())
              };
              if !cont {
                  fdstore.del_client(fd);
              };
          }
          }//Other
    } //match
    } //for
    } //loop
}
