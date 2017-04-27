use std::process;
use std::os::unix::io::{RawFd, AsRawFd};


pub struct PersistentChild {
    cmd: &'static str,
    pub child : process::Child,
}

impl PersistentChild {
    fn spawn(cmd: &str) -> process::Child {
        process::Command::new(cmd)
                .stdin(process::Stdio::piped())
                .stdout(process::Stdio::piped())
                .stderr(process::Stdio::piped())
                .spawn()
                .expect("failed to execute process")
    }

    pub fn new(cmd: &'static str) -> Self {
        let child_proc = Self::spawn(cmd);
        //TODO, if I want to reuse the pipes, I need to keep them open and need to write my custom
        //child spawner -_-
        // If I keep both ends of the pipes open in the main process, I will not be able to read an
        // EOF if the child closes its end of the pipe. Probabaly I stick to the std library and
        //  create new pipes when a child needs to be respawned

        println!("spawned child with pid {}", child_proc.id());
        PersistentChild{ cmd: cmd, child: child_proc }
    }

    pub fn respawn(&mut self) {
        self.child = Self::spawn(self.cmd);
    }

    pub fn stdin_as_mut(&mut self) -> &mut process::ChildStdin {
        self.child.stdin.as_mut().unwrap()
    }

    pub fn stdout_as_mut(&mut self) -> &mut process::ChildStdout {
        self.child.stdout.as_mut().unwrap()
    }

    pub fn stderr_as_mut(&mut self) -> &mut process::ChildStderr {
        self.child.stderr.as_mut().unwrap()
    }


    fn _unwrap_as_raw_fd<T: AsRawFd>(x: &Option<T>) -> RawFd {
        assert!(x.is_some());
        x.as_ref().unwrap().as_raw_fd()
    }

    pub fn is_stdin(&self, i: RawFd) -> bool {
        Self::_unwrap_as_raw_fd(&self.child.stdin) == i
    }

    pub fn is_stdout(&self, i: RawFd) -> bool {
        Self::_unwrap_as_raw_fd(&self.child.stdout) == i
    }

    pub fn is_stderr(&self, i: RawFd) -> bool {
        Self::_unwrap_as_raw_fd(&self.child.stderr) == i
    }

    // returns immediately, does not reap!
    // Child not modified.
    pub fn has_terminated(&self) -> bool{
        let pid = self.child.id();
        ffi::has_terminated(pid)
    }
}


mod ffi{
    use libc::{c_int, pid_t, uid_t, idtype_t, id_t, SIGCHLD};
    use std::mem;

    extern {
        fn waitid(idtype: idtype_t, id: id_t, infop: *mut siginfo_t, options: c_int) -> c_int;
    } 

    #[repr(C)]
    struct siginfo_t {
        pub si_signo: c_int,
        pub si_errno: c_int,
        pub si_code: c_int,
        pub si_trapno: c_int,
        pub si_pid: pid_t,
        pub si_uid: uid_t,
        pub si_status: c_int,
        pub _pad: [c_int; 25],
        // some fields omitted
    }

    pub fn has_terminated(pid: u32) -> bool {
        //let pid = pid as id_t; // std::process::Child::id returns u32 is pid
        //let pid = pid ad id_t;

        assert_eq!(mem::size_of::<siginfo_t>(), sizeof_siginfo_t);

        let mut info: siginfo_t = unsafe { mem::zeroed() };
        let infop: *mut siginfo_t = &mut info;
        let ret = unsafe { waitid(waitpid_P_PID, pid, infop, waitpidoptions_WNOHANG | waitpidoptions_WEXITED | waitpidoptions_WNOWAIT) };
        assert_eq!(ret, 0);
        if info.si_pid != 0 {
            assert_eq!(info.si_pid, pid as pid_t);
            assert_eq!(info.si_signo, SIGCHLD);
            true
        }else{
            false
        }
    }

    #[link(name = "cppdefs", kind="static")]
    extern {
        static sizeof_siginfo_t: usize;

        static waitpidoptions_WNOHANG: c_int;
        static waitpidoptions_WUNTRACED: c_int;
        static waitpidoptions_WCONTINUED: c_int;
        static waitpidoptions_WEXITED: c_int;
        static waitpidoptions_WSTOPPED: c_int;
        static waitpidoptions_WNOWAIT: c_int;

        static waitpid_P_PID: idtype_t;
    }
}
