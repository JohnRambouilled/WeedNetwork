module Client.WeedMonad where

import Types

import Control.Concurrent.STM (TVar)
import Control.Monad.STM
import Control.Monad.Writer
import Control.Monad.Reader


stmLift :: STM a -> WeedMonad a
stmLift = lift.lift

stmRead :: (Client -> TVar a) -> WeedMonad a
stmRead f = stmLift . readTVar =<< lift (asks f)

