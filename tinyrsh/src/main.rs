extern crate tinyrsh;

use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::io::{Read, Write};
use std::os::unix::io::{RawFd, AsRawFd, IntoRawFd};
use tinyrsh::select as select;
use tinyrsh::select::FdSet;


fn greet_client(mut stream: &TcpStream) {
    let remote = stream.peer_addr().unwrap();
    println!("connected: {}", remote);

    match stream.write(b"hello") {
        Err(e) => println!("{}", e),
        Ok(n) => println!("wrote {} bytes", n),
    };
}

fn read_client(mut stream: &TcpStream) -> bool {
    let remote = stream.peer_addr().unwrap();


    let mut buffer = [0; 10];
    let cont = match stream.read(&mut buffer) {
        Err(e) => {
                println!("{}", e);
                false
            },
        Ok(n) => {
                println!("{}: {:?} ({} bytes)", remote, buffer, n);
                n != 0
            },
    };
    cont
}

fn debug_fdset(fdset: &FdSet) {
    let maxfds = select::FD_SETSIZE;
    for i in 0 .. maxfds {
        if fdset.contains(i) {
            println!("contains {}", i);
        }
    }
}

fn main() {
    println!("Hello, world!");
    let listener = TcpListener::bind("127.0.0.1:6699").unwrap();

    let mut all_fdset = FdSet::new();
    let srv_fd = listener.as_raw_fd(); // not consumed
    all_fdset.insert(srv_fd);

    let mut clients = HashMap::new(); //could also use FromRawFd

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

    for stream in listener.incoming() {
        let mut high_fd = 0;
        match stream {
            Ok(mut stream) => {
                greet_client(&mut stream);
                high_fd = std::cmp::max(high_fd, add_client(&mut all_fdset, &mut clients, stream));
            }
            Err(e) => { /* connection failed */ }
        }
        println!("selecting");
        loop {
            let mut fdset = all_fdset.clone();
            let res = select::select(high_fd + 1, Some(&mut fdset), None, None, None);
            println!("selected returned {}", res);
            debug_fdset(&fdset);
            
            let mut accept_new = false;

            let mut handled_fds = 0;
            for i in 0 .. high_fd + 1 {
                if fdset.contains(i) {
                    if i == srv_fd {
                        println!("need to accept new connection");
                        accept_new = true;
                    }else{
                        assert!(clients.contains_key(&i));
                        let cont = {
                            let ref stream = clients[&i];
                            read_client(&stream) 
                        };
                        if !cont {
                            del_client(&mut all_fdset, &mut clients, i);
                        };
                    }
                    handled_fds += 1;
                }
            }
            assert_eq!(handled_fds, res);
            if accept_new {break}
        }
    }
}
