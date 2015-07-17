{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Timer where


import qualified Data.Map as M
import Control.Monad.State hiding (put, get)
import Control.Monad.RWS.Lazy hiding (put, get)
import Control.Monad.Writer hiding (put, get)
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Tuple
import Data.Time hiding (DiffTime)
import Data.Binary

import Client.Class
import Log

type DiffTime = NominalDiffTime

type Time = UTCTime
data TimerEntry = TimerEntry {timerLast :: Time,
                              timerTTL :: DiffTime,
                              timerUnregister :: IOLog Bool}

data Timer = Timer {timerMap :: M.Map Int TimerEntry,
                    timerCount :: [Int]}
--type SW Timer = StateT Timer
--class (MonadState Timer m, LogIO m) => SW Timer m
--instance SW Timer (RWST p Log Timer IO)

instance Show Timer where show tm = show (M.keys .timerMap $ tm, take 10 $ timerCount tm)

getTime :: IO Time
getTime = getCurrentTime


{-| Refreshes the current entry |-}
refreshTimerEntry :: (SW Timer m) => Int -> m () 
refreshTimerEntry index = do (time,tM) <- (,) <$> liftIO getTime <*> gets timerMap
                             modify $ \s -> s{timerMap= M.adjust (updateTime time) index tM}
       where updateTime time e = e{timerLast=time}


registerTimer' :: (SW Timer m) => DiffTime -> IOLog Bool -> m (Int,TimerEntry)
registerTimer' ttl unreg = do (tM,tC) <- (,) <$> gets timerMap <*> (head <$> gets timerCount)
                              time <- liftIO $ getTime 
                              let entry = TimerEntry time ttl unreg
                              modify $ \s -> s{timerMap = M.insert tC entry tM,
                                               timerCount = tail $ timerCount s}
                              return (tC,entry)
                              


registerTimerM :: MVar Timer
               -> DiffTime --The minimum time before calling the function
               -> IOLog (Bool) -- the function called each time the entries expires. Returns True if it is needed to remove it.
               -> IO (IOLog (),IOLog ()) -- The tuple (update function, free function)
registerTimerM timerV ttl unreg = if ttl >= 0 then do ((index,entry),logs) <- modifyMVar timerV $ (formTuple <$>) . runRWST (registerTimer' ttl unreg) ()
                                                      pure (refreshFun index, freeFun index)
                                             else pure $ (pure (), pure ())
        where refreshFun index = tell =<< liftIO (snd <$> runRWSTMVar timerV () (refreshTimerEntry index))
              freeFun index = tell =<< liftIO (snd <$> runRWSTMVar timerV () (freeEntry index))
              formTuple (a,s,w) = (s, (a,w))

                                              

{-
unregisterEntry :: (MonadIO m) => Int -> TimerEntry -> SW Timer m ()
unregisterEntry index entry = do removePred <- liftIO (timerUnregister entry)
                                 if removePred then freeEntry index 
                                               else refreshTimerEntry index
-}

unregisterEntry :: (LogIO m) => MVar Timer -> Int -> TimerEntry -> m ()
unregisterEntry timerV index entry = do 
                                        keepLog TimerLog Normal $ "[timer] entry " ++ show index ++ " has timed out. Calling the callback..."
                                        (removePred, logs) <- liftIO . runWriterT $ timerUnregister entry
                                        tell logs
                                        logs' <- liftIO $ if removePred then snd <$> runRWSTMVar timerV () (freeEntry index) 
                                                         else snd <$> runRWSTMVar timerV () (refreshTimerEntry index)
                                        tell logs'

freeEntry :: SW Timer m => Int -> m ()
freeEntry index = (keepLog TimerLog Normal ("[timer] flushing entrie : " ++ show index)) >>
                  freeEntries >> freeIndex
        where freeIndex = modify $ \s -> s{timerCount = index:timerCount s}
              freeEntries = modify $ \s->s{timerMap = M.delete index (timerMap s)}



{-
checkTimeOut :: (MonadIO m) => SW Timer m ()
checkTimeOut = do (time,tM) <- (,) <$> liftIO getTime <*> gets timerMap
                  forM_ (M.assocs tM) $ checkTime time
    where checkTime time (index,entry) = when ( (diffUTCTime time $ timerLast entry) >  timerTTL entry) $
                                               unregisterEntry index entry

-}

checkTimeOut :: MVar Timer -> IOLog ()
checkTimeOut timerV = do (time,tM) <- liftIO $ (,) <$> getTime <*> (timerMap <$> readMVar timerV)
                         forM_ (M.assocs tM) $ checkTime time
    where checkTime time (index,entry) = when ( (diffUTCTime time $ timerLast entry) >  timerTTL entry) $
                                               unregisterEntry timerV index entry


repeatEach :: MVar Timer -> IOLog () -> DiffTime -> IO (IOLog ())
repeatEach timerV act ttl = snd <$> registerTimerM timerV ttl (act >> pure False)


repeatNTimes :: MVar Timer -> IOLog () -> IOLog () -> DiffTime -> Int -> IO (IOLog ())
repeatNTimes timerV act end freq ntimes = do initTime <- getTime
                                             let act' = do act
                                                           cur <- liftIO getTime
                                                           if diffUTCTime cur initTime >= freq*fromIntegral ntimes
                                                                   then end >> return True
                                                                   else return False
                                             snd <$> registerTimerM timerV freq act'


startTimer :: LogFunction -> MVar Timer -> Int -> IO ()
startTimer logf timer checkfreq = do logf $ [LogMsg Normal TimerLog "\n\n Starting timer check"]
                                     runWriterT (checkTimeOut timer) >>= logf . snd
                                     threadDelay checkfreq >> startTimer logf timer checkfreq

instance Binary UTCTime where
    put (UTCTime d h) = put (fromEnum d) >> put (fromEnum h)
    get = UTCTime <$> (toEnum <$> get) <*> (toEnum <$> get)
