module Client.WeedMonad where

import Types

import Control.Concurrent.STM 
import Control.Monad.STM
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Time.Clock.POSIX


runWM' :: WeedMonad a -> WeedMonad (IO a)
runWM' w = (flip runWM)  w <$> ask

runWM :: Client -> WeedMonad a -> IO a
runWM c w = do t <- getPOSIXTime
               (r, l) <- atomically $ do writeTVar (clTime c) t
                                         (flip runReaderT) c $ runWriterT w
               onWeedOrders l
               pure r


onWeedOrders :: WeedOrders -> IO ()
onWeedOrders = mapM_ onWeedOrder
    where onWeedOrder (WeedLog l) = print l
          onWeedOrder (WeedPerformIO a) = a

logM :: String -> String -> LogStatus -> String -> WeedMonad ()
logM s f l = tell . (:[]) . WeedLog . Log s f l

weedIO :: IOAction -> WeedMonad ()
weedIO = tell. (:[]) . WeedPerformIO

stmRead :: (Client -> TVar a) -> WeedMonad a
stmRead a = readSTM =<< asks a

stmWrite :: (Client -> TVar a) -> a -> WeedMonad ()
stmWrite a v = (flip writeSTM) v =<< asks a

stmModify :: (Client -> TVar a) -> (a -> a) -> WeedMonad ()
stmModify a f = (flip modifySTM) f  =<< asks a

getClient :: WeedMonad Client
getClient = ask

liftSTM :: STM a -> WeedMonad a
liftSTM = lift.lift

readSTM :: TVar a -> WeedMonad a
readSTM = liftSTM . readTVar

writeSTM :: TVar a -> a -> WeedMonad ()
writeSTM v = liftSTM . writeTVar v

modifySTM :: TVar a -> (a -> a) -> WeedMonad ()
modifySTM v = liftSTM . modifyTVar' v

getTime :: WeedMonad Time
getTime = stmRead clTime

