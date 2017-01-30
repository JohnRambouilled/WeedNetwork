module Client.WeedMonad where

import Types

import Control.Concurrent.STM 
import Control.Monad.STM
import Control.Monad.Writer
import Control.Monad.Reader



logM :: String -> String -> LogStatus -> String -> WeedMonad ()
logM s f l = tell . (:[]) . WeedLog . Log s f l

weedIO :: IOAction -> WeedMonad ()
weedIO = tell. (:[]) . WeedPerformIO

stmRead :: (Client -> TVar a) -> WeedMonad a
stmRead f = readSTM =<< lift (asks f)

stmWrite :: (Client -> TVar a) -> a -> WeedMonad ()
stmWrite a v = do tvar <- lift (asks a)
                  liftSTM $ writeTVar tvar v

stmModify :: (Client -> TVar a) -> (a -> a) -> WeedMonad ()
stmModify a f = liftSTM . flip modifyTVar f =<< lift (asks a)

getClient :: WeedMonad Client
getClient = lift $ ask

liftSTM :: STM a -> WeedMonad a
liftSTM = lift.lift

readSTM :: TVar a -> WeedMonad a
readSTM = liftSTM . readTVar

getTime :: WeedMonad Time
getTime = stmRead clTime

