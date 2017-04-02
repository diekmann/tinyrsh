// copy from https://github.com/nix-rust/nix/blob/master/src/sys/select.rs

use std::ptr::null_mut;
use std::os::unix::io::RawFd;
use libc::{c_int, timeval};
use time::TimeVal;

pub const FD_SETSIZE: RawFd = 1024;

#[cfg(any(target_os = "macos", target_os = "ios"))]
#[repr(C)]
#[derive(Clone)]
pub struct FdSet {
    bits: [i32; FD_SETSIZE as usize / 32]
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
const BITS: usize = 32;

#[cfg(not(any(target_os = "macos", target_os = "ios")))]
#[repr(C)]
#[derive(Clone)]
pub struct FdSet {
    bits: [u64; FD_SETSIZE as usize / 64]
}

#[cfg(not(any(target_os = "macos", target_os = "ios")))]
const BITS: usize = 64;

impl FdSet {
    pub fn new() -> FdSet {
        FdSet {
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

    //pub fn contains(&mut self, fd: RawFd) -> bool {
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
        format!("FdSet [{}]", active_fds.join(", "))
    }
}

mod ffi {
    use libc::{c_int, timeval};
    use super::FdSet;

    extern {
        pub fn select(nfds: c_int,
                      readfds: *mut FdSet,
                      writefds: *mut FdSet,
                      errorfds: *mut FdSet,
                      timeout: *mut timeval) -> c_int;
    }
}

pub fn select(nfds: c_int,
              readfds: Option<&mut FdSet>,
              writefds: Option<&mut FdSet>,
              errorfds: Option<&mut FdSet>,
              timeout: Option<&mut TimeVal>) -> c_int {
    let readfds = readfds.map(|set| set as *mut FdSet).unwrap_or(null_mut());
    let writefds = writefds.map(|set| set as *mut FdSet).unwrap_or(null_mut());
    let errorfds = errorfds.map(|set| set as *mut FdSet).unwrap_or(null_mut());
    let timeout = timeout.map(|tv| tv as *mut TimeVal as *mut timeval)
                         .unwrap_or(null_mut());

    let res = unsafe {
        ffi::select(nfds, readfds, writefds, errorfds, timeout)
    };

    res
}
