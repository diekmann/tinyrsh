module Main where

import Data.Maybe (isNothing, isJust)
import Control.Monad (when, forever)
import System.IO
import Network.Socket
import Control.Concurrent

main :: IO ()
main = do
    let port = 6699
    putStrLn "cheap netcat replacement"
    let hints = defaultHints { addrFamily = AF_INET, addrSocketType = Stream, addrProtocol = defaultProtocol }
    res <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
    -- TODO: haha, "localhost" resolves to two addresses. Fix multi addresses returned.
    putStrLn $ show res
    -- assume exactly one result
    let rp = head res
    putStrLn $ "connecting to " ++ show (addrAddress rp)
    sock <- socket (addrFamily rp) (addrSocketType rp) (addrProtocol rp)
    connect sock (addrAddress rp)
    hdl <- socketToHandle sock ReadWriteMode
    hdlEncoding <- hGetEncoding hdl
    putStrLn $ "Are we in binary mode? " ++ show (isNothing hdlEncoding)
    hdlBuff <- hGetBuffering hdl
    putStrLn $ "Have we disabled buffering? " ++ show hdlBuff

    connectionHandler hdl

    hClose hdl


connectionHandler :: Handle -> IO ()
connectionHandler hdl = do
    sockRcvThread <- forkIO (sockrcv hdl)

    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    isTTY <- hIsTerminalDevice stdin
    when isTTY $ do
        putStrLn "connected to terminal, disabling echo"
        hSetEcho stdin False
    stdinReadLoop hdl
    putStrLn "cya"
    
    where sockrcv hdl = forever $ hGetChar hdl >>= hPutChar stdout

stdinReadLoop hdl = do
    hasStdIn <- hIsReadable stdin
    if hasStdIn then do
        --TODO: does putchar on hdl need synchronization with the above forkIO read thread?
        hGetChar stdin >>= hPutChar hdl
        stdinReadLoop hdl
    else
        putStrLn "STDIN closed?"
