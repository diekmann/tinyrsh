# tinyrsh
If you can read this, I never finished what started


### Overview: Basic `nc -e` Implementation
In this small repository, I test different ways to implement a small, insecure, incomplete implementation of a remote shell.
The `rsh-impls` folder contains some different attempts to build a remote shell. 
All implementations start a basic server with `socket`, `bind`, `listen`, `accept`.

 - c-unix
   - low level C implementation
   - only one client at a time
   - forks, dups the socket fd to stdin stdout stderr, cleans other fds, exec /bin/sh
   - problem: C is too much toil

 - hs-unix
   - low level Haskell implementation, following the C implementation
   - problems: when exec-ing /bin/sh, the forked child process is not properly cleaned. Too much Haskell still living in forked child before exec. Haskell library was not really designed for what I'm doing here. Also zombies.

 - hs-idiomatic
   - implementation in idiomatic Haskell
   - Uses System.Process to spawn processes and handles instead of file descriptors
   - (almost) feature complete: reaping zombies, handling multiple connections in parallel, no race condition in signal handling
   - problem: printing low level debug information (pids if childs, ...) is hard. Overall, this is still the nicest implementation

None of these implementation spawns a terminal. It is not possible to do `su` or open a python shell.


### Overview: Get a Terminal
In `py-ptysh`, a very very very hacky helper script to spawn a `/bin/sh` attached to a terminal can be found.
I patched the built-in pty module for this to work.
Reason: when the remote client disconnects, the shell process is not terminated and hanging around indefinitely.
Patch: If STDIN seems closed, proceed to close the whole process. Assumes that `/bin/sh` exits by itself at this point.


### Current Working Version of tinyrsh
In `tinyrsh` resides the current working trunk of the actual implementation.

 - based on `rsh-impls/hs-idiomatic`
 - spawns the python helper to get a shell attached to a terminal
 - everything is line buffered, `man man` does not give a great experience
 - `su` and `python` shell over tinyrsh works
