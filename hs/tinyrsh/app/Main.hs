module Main where

import System.IO
import Network.Socket
import System.Posix.Process (forkProcess, executeFile)
import System.Posix.IO (handleToFd, setFdOption, dupTo, stdInput, stdOutput, stdError, closeFd)
import qualified System.Posix.IO as FdOption (FdOption(..))
import System.Directory (getDirectoryContents)
import qualified System.Posix.Types (Fd)


setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket =
    setFdOption (fromIntegral $ fdSocket socket) FdOption.CloseOnExec True


main :: IO ()
main = do
	putStrLn "Starting"
	srvSock <- socket AF_INET Stream 0
	-- Why does Haskell not support SOCK_CLOEXEC directly?
	setSocketCloseOnExec srvSock
	setSocketOption srvSock ReuseAddr 1
	bind srvSock (SockAddrInet 6699 iNADDR_ANY)
	listen srvSock 1
	sockLoop srvSock

sockLoop :: Socket -> IO ()
sockLoop srvSock = do
	(remoteSock, remoteAddr) <- accept srvSock
	putStrLn $ "client connected: " ++ show remoteAddr
	hdl <- socketToHandle remoteSock ReadWriteMode
	handleClient hdl
	sockLoop srvSock

handleClient :: Handle -> IO ()
handleClient hdl = do
	pid <- forkProcess (doChild hdl)
	putStrLn $ "forked pid: " ++ show pid
	hClose hdl
	--TODO reap zombies!!

doChild :: Handle -> IO ()
doChild hdl = do
	hPutStrLn hdl "Hello World\n"
	sockFd <- handleToFd hdl
	putStrLn $ "sockFd: " ++ show sockFd
	dirFds1 <- getDirectoryContents "/proc/self/fd"
	let dirFds2 = [(read fd)::System.Posix.Types.Fd | fd <- dirFds1, fd `notElem` [".", ".."]]
	putStrLn $ "Open fds: " ++ show dirFds2
	--TODO properly close? what did haskell open??
	--mapM_ closeFd [fd | fd <- dirFds2, fd < sockFd] --TODO also closes socket
	--mapM_ closeFd $ [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]
	dupTo sockFd stdInput
	dupTo sockFd stdOutput
	dupTo sockFd stdError
	-- Somethime we get "tinyrsh-exe: failed to create OS thread: Resource temporarily unavailable"
	-- But the shell spawns up. What is Haskell trying to do?
	executeFile "/home/corny/git/tinyrsh/exectest/exectest" False [] Nothing --"/bin/sh" False [] Nothing
	putStrLn "unreachable"
	

