use std::process;
use std::os::unix::io::{RawFd, AsRawFd};


pub struct PersistentChild {
    cmd: &'static str,
    pub child : process::Child,
}

impl PersistentChild {
    pub fn new(cmd: &'static str) -> Self {
        let child_proc = process::Command::new(cmd)
                .stdin(process::Stdio::piped())
                .stdout(process::Stdio::piped())
                .stderr(process::Stdio::piped())
                .spawn()
                .expect("failed to execute process");

        //TODO, if I want to reuse the pipes, I need to keep them open and need to write my custom
        //child spawner -_-
        // If I keep both ends of the pipes open in the main process, I will not be able to read an
        // EOF if the child closes its end of the pipe. Probabaly I stick to the std library and
        //  create new pipes when a child needs to be respawned

        println!("spawned child with pid {}", child_proc.id());
        PersistentChild{ cmd: cmd, child: child_proc }
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
}
