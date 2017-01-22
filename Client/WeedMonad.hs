module Client.WeedMonad where

import Types

import Control.Concurrent.STM (TVar)
import Control.Monad.STM
import Control.Monad.Writer
import Control.Monad.Reader


liftSTM :: STM a -> WeedMonad a
liftSTM = lift.lift

stmRead :: (Client -> TVar a) -> WeedMonad a
stmRead f = liftSTM . readTVar =<< lift (asks f)

logM :: String -> WeedMonad ()
logM = tell . WeedLog

weedIO :: IOAction -> WeedMonad ()
weedIO = tell . WeedPerformIO
