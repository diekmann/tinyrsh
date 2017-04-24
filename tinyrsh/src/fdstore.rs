
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream, SocketAddr};
use std::io::Read;
use std::io;
use std::process;
use std::os::unix::io::{RawFd, AsRawFd};
use select::FdSet;
use child::PersistentChild;


pub type Clients = HashMap<RawFd, TcpStream>;

pub struct FdStore<Aux> {
    // the listening server socket
    srv_sock: TcpListener,

    // the connected remote clients
    pub clients: Clients, //connected clients

    pub child: PersistentChild,

    //public auxiliary data
    pub aux: Aux,

    // private internal auxiliary data
    all_fdset: FdSet,
}


impl<Aux> FdStore<Aux> {
    pub fn new(srv_sock: TcpListener, child: PersistentChild, aux: Aux) -> Self {
        let mut s = FdStore {srv_sock: srv_sock, clients: HashMap::new(), child: child, aux:aux, all_fdset: FdSet::new()};
        s.update_fdset();
        s
    }

    fn update_fdset(&mut self) {
        self.all_fdset.clear();

        let srv_fd = self.srv_sock.as_raw_fd(); // not consumed
        println!("inserting srv fd {}", srv_fd);
        self.all_fdset.insert(srv_fd);

        for fd in [self.child.child.stdout.as_ref().unwrap().as_raw_fd(), self.child.child.stderr.as_ref().unwrap().as_raw_fd()].iter() {
            println!("inserting child fd {}", fd);
            self.all_fdset.insert(*fd);
        };

        for fd in self.clients.keys() {
            println!("inserting client fd {}", fd);
            self.all_fdset.insert(*fd);
        };
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

    fn child_eof(&mut self) {
        println!("!!!!!!! child io returned EOF.");
        println!("killing old child");
        self.child.child.kill();
        self.child.respawn();
        self.update_fdset();
        //println!("exit code: {}", child.wait().expect("child unexpected state"));
    }

    pub fn select(&mut self,
                  srv_ready: &Fn(OnceAccept, &Clients, &mut Aux) -> io::Result<TcpStream>,
                  remote_ready: &Fn(OnceRead<TcpStream>, &mut process::ChildStdin, &mut Aux) -> bool,
                  child_stdout_ready: &Fn(OnceRead<process::ChildStdout>, &Clients, &mut Aux) -> bool,
                  child_stderr_ready: &Fn(OnceRead<process::ChildStderr>, &Clients, &mut Aux) -> bool,
                  ) -> () {
        let readfds = self.all_fdset.readfds_select();

        let mut respawn_child = false;

        for fd in readfds {
            if fd == self.srv_sock.as_raw_fd() {
                // server socket wants to accept new connection
                match srv_ready(OnceAccept{ listener: &self.srv_sock}, &self.clients, &mut self.aux) {
                  Err(e) => println!("not accepting? {:?}", e),
                  Ok(stream) => {
                      println!("adding new client");
                      self.add_client(stream);
                  }
                };
            } else if self.child.is_stdout(fd) {
                let cont = child_stdout_ready(OnceRead{ r: self.child.stdout_as_mut() }, &self.clients, &mut self.aux);
                if !cont { respawn_child = true; }
            } else if self.child.is_stderr(fd) {
                let cont = child_stderr_ready(OnceRead{ r: self.child.stderr_as_mut() }, &self.clients, &mut self.aux);
                if !cont { respawn_child = true; }
            } else if self.clients.contains_key(&fd){
                // client connection demands attention
                let cont = {
                    let stream: &mut TcpStream = self.clients.get_mut(&fd).unwrap();
                    let r = OnceRead { r: stream };
                    remote_ready(r, self.child.stdin_as_mut(), &mut self.aux)
                };
                if !cont {
                    println!("client exited");
                    self.del_client(fd);
                }
            } else {
                assert!(false, "unknown fd");
            }
        }

        // only after all fds have been handled
        if respawn_child {self.child_eof()}
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

