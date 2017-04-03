extern crate tinyrsh;

use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};
use std::os::unix::io::{RawFd, AsRawFd};
use tinyrsh::select as select;
use tinyrsh::select::FdSet;
use tinyrsh::child::PersistentChild;


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
    println!("{:?} ({} bytes) `{}'", buf, n, String::from_utf8_lossy(&buf[..n]));
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

    let mut all_fdset = FdSet::new();
    let srv_fd = listener.as_raw_fd(); // not consumed
    
    for fd in [srv_fd, child.child.stdout.as_ref().unwrap().as_raw_fd(), child.child.stderr.as_ref().unwrap().as_raw_fd()].iter() {
        println!("inserting fd {}", fd);
        all_fdset.insert(*fd);
    };

    let mut clients: HashMap<RawFd, TcpStream> = HashMap::new(); //could also use FromRawFd

    fn add_client(all_fdset: &mut FdSet, clients: &mut HashMap<RawFd, TcpStream>, stream: TcpStream) -> RawFd {
        let stream_fd: RawFd = stream.as_raw_fd(); //not consume
        println!("inserted fd {}", stream_fd);
        clients.insert(stream_fd, stream);
        all_fdset.insert(stream_fd);
        stream_fd
    };
    fn del_client(all_fdset: &mut FdSet, clients: &mut HashMap<RawFd, TcpStream>, stream_fd: RawFd) {
        assert!(clients.contains_key(&stream_fd));
        //let stream = clients.get(&stream_fd);
        println!("removing fd {}", stream_fd);
        assert!(all_fdset.contains(stream_fd));
        all_fdset.remove(stream_fd);
        assert!(clients.contains_key(&stream_fd));
        clients.remove(&stream_fd);
    };

   println!("select-looping");
   loop {
       let (num_active, fdset) = all_fdset.readfds_select();
       assert!(num_active > 0);
       println!("selected returned {}", num_active);
       println!("fdset debug: {}", fdset.debug());

       let mut accept_new = false;

       let mut handled_fds = 0;
       for i in 0 .. fdset.high_fd + 1 {
           if fdset.contains(i) {
               if i == srv_fd {
                   println!("need to accept new connection");
                   accept_new = true;
                   //TODO move code?
               } else if child.is_stdout(i) {
                   println!("child fd {} (stdout) got active", i);
                   let child_eof = forward_to_remotes(child.stdout_as_mut(), &clients);
                   if child_eof {
                       println!("!!!!!!! child exited. pipe will break now");
                       //println!("exit code: {}", child.wait().expect("child unexpected state"));
                       //ahh, ownership!
                   }
                   assert!(!child_eof, "child exited, unhandled!");
               } else if child.is_stderr(i) {
                   println!("child fd {} (stderr) got active", i);
                   read_child(child.stderr_as_mut());
               } else {
                   assert!(clients.contains_key(&i));
                   let cont = {
                       //let ref stream = clients[&i];
                       let stream: &mut TcpStream= clients.get_mut(&i).unwrap();
                       //read_client(stream)
                       copy_to(stream, child.stdin_as_mut())
                   };
                   if !cont {
                       del_client(&mut all_fdset, &mut clients, i);
                   };
               }
               handled_fds += 1;
           }
       }
       assert_eq!(handled_fds, num_active);

       //accept new connection
       if accept_new {
           match listener.accept() {
               Err(e) => println!("couldn't get client: {:?}", e),
               Ok((mut stream, addr)) => {
                   println!("new client: {:?}", addr);
                   greet_client(&mut stream);
                   add_client(&mut all_fdset, &mut clients, stream);
               }
           }
       }
    }
}
