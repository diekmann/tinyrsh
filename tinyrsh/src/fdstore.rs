
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream, SocketAddr};
use std::io::{Read, Write};
use std::io;
use std::process;
use std::os::unix::io::{RawFd, AsRawFd};
use select::FdSet;


pub struct FdStore {
    // the listening server socket
    srv_sock: TcpListener,

    // the connected remote clients
    pub clients: HashMap<RawFd, TcpStream>, //connected clients

    //TODO add child too


    // private auxiliary data
    //TODO make private once we got child in
    pub all_fdset: FdSet,
}


impl FdStore {
    pub fn new(srv_sock: TcpListener) -> Self {
        let srv_fd = srv_sock.as_raw_fd(); // not consumed
        let mut s = FdStore {srv_sock: srv_sock, clients: HashMap::new(), all_fdset: FdSet::new()};
        println!("inserting srv fd {}", srv_fd);
        s.all_fdset.insert(srv_fd);
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
                  srv_ready: &Fn(&Self, OnceAccept) -> io::Result<TcpStream>,
                  child_stdin_hack: &mut process::ChildStdin, //hack to pass to following function
                  remote_ready: &Fn(OnceRead<TcpStream>, &mut process::ChildStdin) -> bool,
                  ) -> Vec<OnceAction> {
        let mut active = vec![];
        let readfds = self.all_fdset.readfds_select();

        for fd in readfds {
            if fd == self.srv_sock.as_raw_fd() {
                // server socket wants to accept new connection
                match srv_ready(self, OnceAccept{ listener: &self.srv_sock}) {
                  Err(e) => println!("not accepting? {:?}", e),
                  Ok(stream) => {
                      println!("adding new client");
                      self.add_client(stream);
                  }
                };
            } else if self.clients.contains_key(&fd){
                // client connection demands attention
                let cont = {
                    let stream: &mut TcpStream = self.clients.get_mut(&fd).unwrap();
                    let r = OnceRead { r: stream };
                    remote_ready(r, child_stdin_hack)
                };
                if !cont {
                    println!("client exited");
                    self.del_client(fd);
                }
            } else {
                active.push(OnceAction::Other(fd));
            }
        }
    active
    }
}

pub struct OnceAccept<'a>{ listener: &'a TcpListener }
pub struct OnceRead<'a, T: Read + 'a>{ r: &'a mut T }

pub enum OnceAction {
    Other(RawFd),
}

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

