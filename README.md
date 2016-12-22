# tinyrsh
If you can read this, I never finished what started


### Overview
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
