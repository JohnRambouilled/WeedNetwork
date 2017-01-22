module Client.WeedMonad where

import Types

import Control.Concurrent.STM 
import Control.Monad.STM
import Control.Monad.Writer
import Control.Monad.Reader


liftSTM :: STM a -> WeedMonad a
liftSTM = lift.lift

stmRead :: (Client -> TVar a) -> WeedMonad a
stmRead f = liftSTM . readTVar =<< lift (asks f)

logM :: String -> String -> LogStatus -> String -> WeedMonad ()
logM s f l = tell . (:[]) . WeedLog . Log s f l

weedIO :: IOAction -> WeedMonad ()
weedIO = tell. (:[]) . WeedPerformIO

stmModify :: (Client -> TVar a) -> (a -> a) -> WeedMonad ()
stmModify a f = liftSTM . flip modifyTVar f =<< lift (asks a)
