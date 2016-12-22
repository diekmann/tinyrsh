module Main where

import Data.Maybe (isNothing, isJust)
import System.IO
import Network.Socket
import qualified System.Process as SysProc
import qualified System.Process.Internals as SysProcInt (withProcessHandle, ProcessHandle__(..))
import Control.Concurrent.MVar
import qualified System.Posix.Signals as Sig

main :: IO ()
main = do
    putStrLn "Starting"
    srvSock <- socket AF_INET Stream 0
    setSocketOption srvSock ReuseAddr 1
    bind srvSock (SockAddrInet 6699 iNADDR_ANY)
    listen srvSock 1
    runningProcesses <- newMVar []
    Sig.installHandler Sig.sigCHLD (Sig.Catch (sigCHLDreaper runningProcesses)) Nothing
    sockLoop srvSock runningProcesses


sockLoop :: Socket -> MVar [SysProc.ProcessHandle] -> IO ()
sockLoop srvSock runningProcesses = do
    (remoteSock, remoteAddr) <- accept srvSock
    putStrLn $ "client connected: " ++ show remoteAddr
    hdl <- socketToHandle remoteSock ReadWriteMode
    ph <- handleClient hdl
    modifyMVar_ runningProcesses (\rp -> return (ph:rp))
    modifyMVar_ runningProcesses reapAndPrint
    sockLoop srvSock runningProcesses


sigCHLDreaper :: MVar [SysProc.ProcessHandle] -> IO ()
sigCHLDreaper runningProcesses =  do
    rp <- takeMVar runningProcesses
    putStrLn "SIGCHLD"
    rp <- reapAndPrint rp
    putMVar runningProcesses rp

--Helper, TODO move to lib
reapAndPrint :: [SysProc.ProcessHandle] -> IO [SysProc.ProcessHandle]
reapAndPrint [] = return []
reapAndPrint phs = do
    exs <- (mapM SysProc.getProcessExitCode phs)
    let handlesAndExitCodes = zip phs exs
    putStrLn "Child status:"
    mapM_ printExitCode handlesAndExitCodes
    --only keep those which have not yet exited
    return [ph | (ph, exitcode) <- handlesAndExitCodes, isNothing exitcode]
    where printExitCode (ph, Nothing) = getPid ph >>= \s -> putStrLn $ "  "++ s ++ "  running"
          printExitCode (ph, (Just exitcode)) = getPid ph >>= \s -> putStrLn $ "  " ++ s ++ " exited: " ++ show exitcode
          getPid ph = SysProcInt.withProcessHandle ph (\phint -> case phint of
                                                                    SysProcInt.OpenHandle h -> return ("[" ++ show h ++ "]")
                                                                    SysProcInt.ClosedHandle _ -> return "")

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

