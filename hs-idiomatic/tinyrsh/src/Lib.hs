module Lib
    ( setSocketCloseOnExec
    ) where

import Network.Socket
import System.Posix.IO (setFdOption)
import qualified System.Posix.IO as FdOption (FdOption(..))


-- Why does Haskell not support SOCK_CLOEXEC directly?
setSocketCloseOnExec :: Socket -> IO ()
setSocketCloseOnExec socket =
    setFdOption (fromIntegral $ fdSocket socket) FdOption.CloseOnExec True

