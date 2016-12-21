module Main where

import System.IO
import Network.Socket
import qualified System.Process as SysProc
import qualified Lib


main :: IO ()
main = do
    putStrLn "Starting"
    srvSock <- socket AF_INET Stream 0
    Lib.setSocketCloseOnExec srvSock
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
    let shProcess = (SysProc.proc "/bin/sh" []){ SysProc.std_out = SysProc.UseHandle hdl,
                                                 SysProc.std_in = SysProc.UseHandle hdl,
                                                 SysProc.std_err = SysProc.UseHandle hdl }
    (cin, cout, cerr, ph) <- SysProc.createProcess shProcess
    putStrLn $ "created Process " ++ show cin
    --TODO: reap zombies, make interactive python spawning possible.

