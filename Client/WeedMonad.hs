{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Client.WeedMonad where

import Types

import Control.Concurrent.STM 
import Control.Monad.STM
import Control.Monad.Trans
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Reader as R
import Data.Time.Clock.POSIX


instance MonadRandom WeedMonad where
  putStdGen = stmWrite clRndGen
  asksStdGen = stmRead clRndGen


runWM :: Client -> WeedMonad a -> IO a
runWM c w = do t <- getPOSIXTime
               (r, l) <- atomically $ do writeTVar (clTime c) t
                                         (flip R.runReaderT) c . W.runWriterT $ runWeedMonad w
               onWeedOrders (clLogHandler c) l
               pure r

runWM' :: WeedMonad a -> WeedMonad (IO a)
runWM' w = (flip runWM)  w <$> ask

onWeedOrders :: (Log -> IO ()) -> WeedOrders -> IO ()
onWeedOrders logH = mapM_ onWeedOrder . runWeedOrders 
    where onWeedOrder (WeedLog l) = logH l
          onWeedOrder (WeedPerformIO a) = a

logM :: String -> String -> LogStatus -> String -> WeedMonad ()
logM s f l = tell . WeedLog . Log s f l

weedIO :: IOAction -> WeedMonad ()
weedIO = tell. WeedPerformIO

stmRead :: (Client -> TVar a) -> WeedMonad a
stmRead a = readSTM =<< asks a

stmWrite :: (Client -> TVar a) -> a -> WeedMonad ()
stmWrite a v = (flip writeSTM) v =<< asks a

stmModify :: (Client -> TVar a) -> (a -> a) -> WeedMonad ()
stmModify a f = (flip modifySTM) f  =<< asks a

  
getClient :: WeedMonad Client
getClient = ask

liftSTM :: STM a -> WeedMonad a
liftSTM = WeedMonad . lift . lift

readSTM :: TVar a -> WeedMonad a
readSTM = liftSTM . readTVar

writeSTM :: TVar a -> a -> WeedMonad ()
writeSTM v = liftSTM . writeTVar v

modifySTM :: TVar a -> (a -> a) -> WeedMonad ()
modifySTM v = liftSTM . modifyTVar' v

getTime :: WeedMonad Time
getTime = stmRead clTime


asks :: (Client -> a) -> WeedMonad a
asks = (<$> ask)

ask :: WeedMonad Client
ask = WeedMonad R.ask
  
tell :: WeedOrder -> WeedMonad ()
tell = WeedMonad . W.tell . WeedOrders . pure

