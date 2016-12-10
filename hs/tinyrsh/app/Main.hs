module Main where

import System.IO
import Network.Socket
import System.Posix.Process (forkProcess, executeFile)
import System.Posix.IO (handleToFd, dupTo, stdInput, stdOutput, stdError, closeFd)
import System.Directory (getDirectoryContents)
import qualified System.Posix.Types (Fd)

main :: IO ()
main = do
	putStrLn "Starting"
	srvSock <- socket AF_INET Stream 0
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
	--mapM_ closeFd $ map (read::String -> System.Posix.Types.Fd) ["0","1","2","3","4","5","6","7","8","9","10","11","12","13", "14"]
	dupTo sockFd stdInput
	dupTo sockFd stdOutput
	dupTo sockFd stdError
	executeFile "/bin/sh" False [] Nothing
	putStrLn "unreachable"
	

