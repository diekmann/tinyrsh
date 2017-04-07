// copy from https://github.com/nix-rust/nix/blob/master/src/sys/select.rs

use std::ptr::null_mut;
use std::os::unix::io::RawFd;
use libc::{c_int, timeval};
use time::TimeVal;
use std::cmp::max;

pub const FD_SETSIZE: RawFd = 1024;

#[cfg(any(target_os = "macos", target_os = "ios"))]
#[repr(C)]
#[derive(Clone)]
pub struct RawFdSet {
    bits: [i32; FD_SETSIZE as usize / 32]
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
const BITS: usize = 32;

#[cfg(not(any(target_os = "macos", target_os = "ios")))]
#[repr(C)]
#[derive(Clone)]
pub struct RawFdSet {
    bits: [u64; FD_SETSIZE as usize / 64]
}

#[cfg(not(any(target_os = "macos", target_os = "ios")))]
const BITS: usize = 64;

impl RawFdSet {
    pub fn new() -> RawFdSet {
        RawFdSet {
            bits: [0; FD_SETSIZE as usize / BITS]
        }
    }

    pub fn insert(&mut self, fd: RawFd) {
        let fd = fd as usize;
        self.bits[fd / BITS] |= 1 << (fd % BITS);
    }

    pub fn remove(&mut self, fd: RawFd) {
        let fd = fd as usize;
        self.bits[fd / BITS] &= !(1 << (fd % BITS));
    }

    pub fn contains(& self, fd: RawFd) -> bool {
        let fd = fd as usize;
        self.bits[fd / BITS] & (1 << (fd % BITS)) > 0
    }

    pub fn clear(&mut self) {
        for bits in &mut self.bits {
            *bits = 0
        }
    }

    pub fn debug(&self) -> String {
        let mut active_fds = vec![];
        for i in 0 .. FD_SETSIZE {
            if self.contains(i) {
                active_fds.push(format!("{}", i));
            }
        }
        format!("RawFdSet [{}] (maxfd: {})", active_fds.join(", "), self.compute_max_fd())
    }

    fn compute_max_fd(&self) -> c_int {
        let mut maxfd = 0;
        for i in 0 .. FD_SETSIZE {
            if self.contains(i) {
                maxfd = i;
            }
        }
        maxfd
    }
}

mod ffi {
    use libc::{c_int, timeval};
    use super::RawFdSet;

    extern {
        pub fn select(nfds: c_int,
                      readfds: *mut RawFdSet,
                      writefds: *mut RawFdSet,
                      errorfds: *mut RawFdSet,
                      timeout: *mut timeval) -> c_int;
    }
}

pub fn select(nfds: c_int,
              readfds: Option<&mut RawFdSet>,
              writefds: Option<&mut RawFdSet>,
              errorfds: Option<&mut RawFdSet>,
              timeout: Option<&mut TimeVal>) -> c_int {
    let readfds = readfds.map(|set| set as *mut RawFdSet).unwrap_or(null_mut());
    let writefds = writefds.map(|set| set as *mut RawFdSet).unwrap_or(null_mut());
    let errorfds = errorfds.map(|set| set as *mut RawFdSet).unwrap_or(null_mut());
    let timeout = timeout.map(|tv| tv as *mut TimeVal as *mut timeval)
                         .unwrap_or(null_mut());

    let res = unsafe {
        ffi::select(nfds, readfds, writefds, errorfds, timeout)
    };

    res
}


//TODO this is a stupid iterator
pub struct FdSetIter {
    raw_fd_set: RawFdSet,
    high_fd: c_int, // iter to +1
    next_fd: c_int,
}

impl Iterator for FdSetIter {
    type Item = RawFd;

    fn next(&mut self) -> Option<RawFd> {
        while self.next_fd < self.high_fd + 1 {
            let cur_fd = self.next_fd;
            self.next_fd += 1;
            if self.raw_fd_set.contains(cur_fd){
                return Some(cur_fd as RawFd);
            }
        }
        None
    }
}


#[derive(Clone)]
pub struct FdSet {
    pub raw_fd_set: RawFdSet,
    pub high_fd: c_int,
}

impl FdSet {
    pub fn new() -> Self {
        FdSet { raw_fd_set: RawFdSet::new(), high_fd: 0 }
    }

    pub fn insert(&mut self, fd: RawFd) {
        self.raw_fd_set.insert(fd);
        self.high_fd = max(self.high_fd, fd);
    }

    pub fn remove(&mut self, fd: RawFd) {
        self.raw_fd_set.remove(fd);
        if fd >= self.high_fd {
            self.high_fd =  self.raw_fd_set.compute_max_fd();
        }
    }

    pub fn contains(&self, fd: RawFd) -> bool {
        self.raw_fd_set.contains(fd)
    }

    pub fn clear(&mut self) {
        self.raw_fd_set.clear();
        self.high_fd = 0;
    }

    pub fn debug(&self) -> String {
        let maxfd = self.raw_fd_set.compute_max_fd();
        assert_eq!(maxfd, self.high_fd);
        format!("{} (highfd: {})", self.raw_fd_set.debug(), self.high_fd)
    }

    pub fn readfds_select(&self) -> FdSetIter {
        let mut ret = FdSetIter {
            raw_fd_set: self.raw_fd_set.clone(),
            high_fd: self.high_fd,
            next_fd: 0,
        };
        let numactive = select(self.high_fd + 1, Some(&mut ret.raw_fd_set), None, None, None);
        let maxfd = ret.raw_fd_set.compute_max_fd();
        assert!(maxfd <= self.high_fd);
        println!("readfds_select {} active: {} (all high: {})", numactive, ret.raw_fd_set.debug(), self.high_fd);
        //(numactive, FdSet{ raw_fd_set: rfds, high_fd: maxfd })
        ret
    }
}





