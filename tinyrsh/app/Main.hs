module Main where

import Data.Maybe (isNothing, isJust)
import System.IO hiding (stdin, stdout)
import Network.Socket
import qualified System.Process as SysProc
import qualified System.Process.Internals as SysProcInt (withProcessHandle, ProcessHandle__(..))
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, ThreadId)
import qualified System.Posix.Signals as Sig
import qualified System.Posix.IO as PIO
import System.Posix.Types (Fd)


--TODO also add stderr
data StdIOSet = StdIOSet SysProc.ProcessHandle -- handle to spawned command
                         (Handle, ThreadId)    -- stdin_w and copy loop
                         (Handle, ThreadId)    -- stdout_r and copy loop
			 -- TODO: make lists?
			 (MVar (Handle))  -- stdin_r attached clients
			 (MVar (Handle))  -- stdout_r attached clients

updateMVarMaybe :: MVar a -> a -> IO ()
updateMVarMaybe mvar val = do
    putStrLn "updateMVarMaybe"
    can <- tryTakeMVar mvar
    case can of Nothing -> putStrLn "cannot take MVar"
                Just x -> putStrLn "can take MVar, old value present"
    putMVar mvar val

addHandle :: StdIOSet -> Handle -> IO ()
addHandle (StdIOSet ph hin hout mvar_in mvar_out) hdl = do
        putStrLn "Adding handle to MVars"
        updateMVarMaybe mvar_in (hdl)
        updateMVarMaybe mvar_out (hdl)

spawnCMD :: String -> IO StdIOSet
spawnCMD cmd = do
    (p_stdin_r, p_stdin_w) <- PIO.createPipe >>= mapM2 PIO.fdToHandle
    (p_stdout_r, p_stdout_w) <- PIO.createPipe >>= mapM2 PIO.fdToHandle
    --(p_stderr_r, p_stderr_w) <- PIO.createPipe >>= mapM2 PIO.fdToHandle
    --TODO API supports pipe creation already. but it did not work -_-
    let shProcess = (SysProc.proc cmd []){ -- SysProc.std_in = SysProc.CreatePipe, does not work
                                           -- SysProc.std_in = SysProc.UseHandle stdin, this works
                                           SysProc.std_in = SysProc.UseHandle p_stdin_r,
                                           SysProc.std_out = SysProc.UseHandle p_stdout_w,
                                           --SysProc.std_err = SysProc.UseHandle p_stderr_w,
                                           SysProc.close_fds = False --TODO close
                                          }
    (_, Nothing, Nothing, ph) <- SysProc.createProcess shProcess
    details <- getPid ph
    putStrLn $ "created Process " ++ details
    hClose p_stdin_r
    hClose p_stdout_w

    --TODO tune
    hSetBuffering p_stdin_w LineBuffering
    hSetBuffering p_stdout_r LineBuffering
    --debug
    l <- hGetLine p_stdout_r 
    putStrLn $ show l
    _ <- hPutStrLn p_stdin_w "test"
    hFlush p_stdin_w
    putStrLn "reading back"
    l <- hGetLine p_stdout_r 
    putStrLn $ show l


    stdin_clients <- newEmptyMVar
    stdout_clients <- newEmptyMVar

    stdin_t <- forkIO (copyMVarFromHdlLoop stdin_clients p_stdin_w)
    stdout_t <- forkIO (copyMVarToHdlLoop p_stdout_r stdout_clients)

    let server_side = StdIOSet ph (p_stdin_w, stdin_t) (p_stdout_r, stdout_t) stdin_clients stdout_clients
    return $ server_side
    where mapM2 f (a, b) = do
            a' <- f a
            b' <- f b
            return (a', b')
          getPid ph = SysProcInt.withProcessHandle ph (\phint -> case phint of
                                                                      SysProcInt.OpenHandle h -> return ("[" ++ show h ++ "]")
                                                                      SysProcInt.ClosedHandle h -> return "?")


main :: IO ()
main = do
    putStrLn "Starting cmd"
    runningProcesses <- newMVar []
    Sig.installHandler Sig.sigCHLD (Sig.CatchInfo (sigCHLDreaper runningProcesses)) Nothing
    --spawn exactly one shell first! let processes reattach. multiple connections is super racy because both connect to the same shell :D
    child <- spawnCMD "../py-ptysh/ptysh.py" 
    --TODO add child to runningProcesses, store full StdIOSet there
    --modifyMVar_ runningProcesses (\rp -> return (child:rp))
    putStrLn "Starting server"
    srvSock <- socket AF_INET Stream 0
    setSocketOption srvSock ReuseAddr 1
    bind srvSock (SockAddrInet 6699 (tupleToHostAddress (127, 0, 0, 1)))
    listen srvSock 1
    sockLoop srvSock child runningProcesses


--TODO remove StdIOSet? make generic for multiple processes
sockLoop :: Socket -> StdIOSet -> MVar [SysProc.ProcessHandle] -> IO ()
sockLoop srvSock childproc runningProcesses = do
    (remoteSock, remoteAddr) <- accept srvSock
    putStrLn $ "client connected: " ++ show remoteAddr
    hdl <- socketToHandle remoteSock ReadWriteMode
    _ <- handleClient childproc hdl
    --modifyMVar_ runningProcesses (\rp -> return (ph:rp))
    modifyMVar_ runningProcesses reapAndPrint
    sockLoop srvSock childproc runningProcesses


sigCHLDreaper :: MVar [SysProc.ProcessHandle] -> Sig.SignalInfo -> IO ()
sigCHLDreaper runningProcesses si =  do
    rp <- takeMVar runningProcesses
    putStrLn $ "SIGCHLD " ++ show ((Sig.siginfoPid (Sig.siginfoSpecific si))) ++ " " ++  show (Sig.siginfoStatus (Sig.siginfoSpecific si))
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
    where printExitCode (ph, Nothing) = getPid ph >>= \s -> putStrLn $ "  "++ s ++ " running"
          printExitCode (ph, (Just exitcode)) = getPid ph >>= \s -> putStrLn $ "  " ++ s ++ " exited: " ++ show exitcode
          getPid ph = SysProcInt.withProcessHandle ph (\phint -> case phint of
                                                                    SysProcInt.OpenHandle h -> return ("[" ++ show h ++ "]")
                                                                    SysProcInt.ClosedHandle h -> return "?")

handleClient :: StdIOSet -> Handle -> IO ()
handleClient child_proc hdl = do
    putStrLn $ "handle client"
    --HANDLE!
    putStrLn $ "TODO"
    addHandle child_proc hdl


copyMVarToHdlLoop :: Handle -> MVar Handle -> IO ()
copyMVarToHdlLoop hdl_from mvar_hdl_to = do
    hdl_to <- takeMVar mvar_hdl_to
    cont <- copyChar hdl_from hdl_to
    case cont of Just _ -> putMVar mvar_hdl_to hdl_to
                 Nothing -> putStrLn "copyMVarToHdlLoop no putMVar"
    copyMVarToHdlLoop hdl_from mvar_hdl_to

copyMVarFromHdlLoop :: MVar Handle -> Handle -> IO ()
copyMVarFromHdlLoop mvar_hdl_from hdl_to = do
    hdl_from <- takeMVar mvar_hdl_from
    cont <- copyChar hdl_from hdl_to
    case cont of Just _ -> putMVar mvar_hdl_from hdl_from
                 Nothing -> putStrLn "copyMVarFromHdlLoop no putMVar"
    copyMVarFromHdlLoop mvar_hdl_from hdl_to


copyChar :: Handle -> Handle -> IO (Maybe Char)
copyChar hdl_from hdl_to = do
    hasChar <- tryGetChar hdl_from
    case hasChar of
        Just c -> do
             --TODO: does putchar on hdl need synchronization with the above forkIO read thread?
            hPutChar hdl_to c
            hFlush hdl_to
            return (Just c)
        Nothing -> do putStrLn "handle closed?"
                      return Nothing


--CHEAP COPY from Client. Not sure about this!
copyHdlLoop hdl_from hdl_to = do
    hasChar <- tryGetChar hdl_from
    case hasChar of
        Just c -> do
             --TODO: does putchar on hdl need synchronization with the above forkIO read thread?
            hPutChar hdl_to c
            hFlush hdl_to
            copyHdlLoop hdl_from hdl_to
        Nothing -> putStrLn "handle closed?"

tryGetChar :: Handle -> IO (Maybe Char)
tryGetChar hdl = do 
    canRead <- hIsReadable hdl
    if canRead then do
        hdlEOF <- hIsEOF hdl
        if (not hdlEOF) then do
            c <- hGetChar hdl
            return (Just c)
        else
            return Nothing
    else
        return Nothing
