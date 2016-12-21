module Main where

import Data.Maybe (isNothing, isJust)
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
    sockLoop srvSock []

sockLoop :: Socket -> [SysProc.ProcessHandle] -> IO ()
sockLoop srvSock runningProcesses = do
    (remoteSock, remoteAddr) <- accept srvSock
    runningProcesses <- reapAndPrint runningProcesses
    putStrLn $ "client connected: " ++ show remoteAddr
    hdl <- socketToHandle remoteSock ReadWriteMode
    ph <- handleClient hdl
    sockLoop srvSock (ph:runningProcesses)

reapAndPrint :: [SysProc.ProcessHandle] -> IO [SysProc.ProcessHandle]
reapAndPrint phs = do
    exs <- mapM SysProc.getProcessExitCode phs
    mapM_ printExitCode exs
    --only keep those which have not yet exited
    return [ph | (ph, exitcode) <- zip phs exs, isNothing exitcode]
    where printExitCode Nothing = putStrLn "  still running"
          printExitCode (Just exitcode) = putStrLn ("  exited: " ++ show exitcode)

handleClient :: Handle -> IO SysProc.ProcessHandle
handleClient hdl = do
    putStrLn $ "handle client"
    let shProcess = (SysProc.proc "/bin/sh" []){ SysProc.std_in = SysProc.UseHandle hdl,
                                                 SysProc.std_out = SysProc.UseHandle hdl,
                                                 SysProc.std_err = SysProc.UseHandle hdl,
                                                 SysProc.close_fds = True }
    (Nothing, Nothing, Nothing, ph) <- SysProc.createProcess shProcess
    putStrLn $ "created Process"
    return ph

