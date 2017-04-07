
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream, SocketAddr};
use std::io::Read;
use std::io;
use std::process;
use std::os::unix::io::{RawFd, AsRawFd};
use select::FdSet;
use child::PersistentChild;


pub struct FdStore {
    // the listening server socket
    srv_sock: TcpListener,

    // the connected remote clients
    pub clients: HashMap<RawFd, TcpStream>, //connected clients

    pub child: PersistentChild,

    // private auxiliary data
    all_fdset: FdSet,
}


impl FdStore {
    pub fn new(srv_sock: TcpListener, child: PersistentChild) -> Self {
        let srv_fd = srv_sock.as_raw_fd(); // not consumed
        let mut s = FdStore {srv_sock: srv_sock, clients: HashMap::new(), child: child, all_fdset: FdSet::new()};
        println!("inserting srv fd {}", srv_fd);
        s.all_fdset.insert(srv_fd);
        for fd in [s.child.child.stdout.as_ref().unwrap().as_raw_fd(), s.child.child.stderr.as_ref().unwrap().as_raw_fd()].iter() {
            println!("inserting child fd {}", fd);
            s.all_fdset.insert(*fd);
        };
        s
    }

    pub fn add_client(&mut self, client: TcpStream) {
        let stream_fd: RawFd = client.as_raw_fd(); //not consume
        println!("inserted fd {}", stream_fd);
        self.clients.insert(stream_fd, client);
        self.all_fdset.insert(stream_fd);
    }

    pub fn del_client(&mut self, client_fd: RawFd) {
        println!("removing fd {}", client_fd);
        assert!(self.clients.contains_key(&client_fd));
        assert!(self.all_fdset.contains(client_fd));
        self.all_fdset.remove(client_fd);
        self.clients.remove(&client_fd);
    }

    pub fn select(&mut self,
                  srv_ready: &Fn(OnceAccept, &Self) -> io::Result<TcpStream>,
                  remote_ready: &Fn(OnceRead<TcpStream>, &mut process::ChildStdin) -> bool,
                  child_stdout_ready: &Fn(OnceRead<process::ChildStdout>, &HashMap<RawFd, TcpStream>) -> (),
                  child_stderr_ready: &Fn(OnceRead<process::ChildStderr>, &HashMap<RawFd, TcpStream>) -> (),
                  ) -> () {
        let readfds = self.all_fdset.readfds_select();

        for fd in readfds {
            if fd == self.srv_sock.as_raw_fd() {
                // server socket wants to accept new connection
                match srv_ready(OnceAccept{ listener: &self.srv_sock}, self) {
                  Err(e) => println!("not accepting? {:?}", e),
                  Ok(stream) => {
                      println!("adding new client");
                      self.add_client(stream);
                  }
                };
            } else if self.child.is_stdout(fd) {
                child_stdout_ready(OnceRead{ r: self.child.stdout_as_mut() }, &self.clients);
            } else if self.child.is_stderr(fd) {
                child_stderr_ready(OnceRead{ r: self.child.stderr_as_mut() }, &self.clients);
            } else if self.clients.contains_key(&fd){
                // client connection demands attention
                let cont = {
                    let stream: &mut TcpStream = self.clients.get_mut(&fd).unwrap();
                    let r = OnceRead { r: stream };
                    remote_ready(r, self.child.stdin_as_mut())
                };
                if !cont {
                    println!("client exited");
                    self.del_client(fd);
                }
            } else {
                assert!(false, "unknown fd");
            }
        }
    }
}

pub struct OnceAccept<'a>{ listener: &'a TcpListener }
pub struct OnceRead<'a, T: Read + 'a>{ r: &'a mut T }

impl <'a>OnceAccept<'a> {
    pub fn do_accept(self) -> io::Result<(TcpStream, SocketAddr)> {
        self.listener.accept()
    }
}

impl <'a, T: Read>OnceRead<'a, T> {
    pub fn do_read(self, buf: &mut [u8]) -> io::Result<usize> {
        self.r.read(buf)
    }
}

