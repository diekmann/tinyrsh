
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};
use std::os::unix::io::{RawFd, AsRawFd};
use select::FdSet;


// RawFd -> Some<io thing>

pub struct FdStore {
    pub srv_sock: TcpListener,
    pub clients: HashMap<RawFd, TcpStream>, //connected clients
    //TODO add child too

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
}
