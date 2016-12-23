module Main where

import Data.Maybe (isNothing, isJust)
import Control.Monad (when)
import System.IO
import Network.Socket

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

    connectionLoop hdl

    hClose hdl


connectionLoop :: Handle -> IO ()
connectionLoop hdl = hGetLine hdl >>= putStrLn
    -- TODO: connect stdin and hdl
    -- read:
    -- http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/telnet-conduit.html
