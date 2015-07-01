module Timer where


import qualified Data.Map as M
import Control.Monad.State hiding (put, get)
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
                              timerUnregister :: IO Bool}

data Timer = Timer {timerMap :: M.Map Int TimerEntry,
                    timerCount :: [Int]}
type TimerT = StateT Timer

instance Show Timer where show tm = show (M.keys .timerMap $ tm, take 10 $ timerCount tm)

getTime :: IO Time
getTime = getCurrentTime


{-| Refreshes the current entry |-}
refreshTimerEntry :: (MonadIO m) => Int -> TimerT m () 
refreshTimerEntry index = do (time,tM) <- (,) <$> liftIO getTime <*> gets timerMap
                             modify $ \s -> s{timerMap= M.adjust (updateTime time) index tM}
       where updateTime time e = e{timerLast=time}


registerTimer' :: (MonadIO m) => DiffTime -> IO Bool -> TimerT m (Int,TimerEntry)
registerTimer' ttl unreg = do (tM,tC) <- (,) <$> gets timerMap <*> (head <$> gets timerCount)
                              time <- liftIO $ getTime 
                              let entry = TimerEntry time ttl unreg
                              modify $ \s -> s{timerMap = M.insert tC entry tM,
                                               timerCount = tail $ timerCount s}
                              return (tC,entry)
                              


registerTimerM :: MVar Timer
               -> DiffTime --The minimum time before calling the function
               -> IO Bool -- the function called each time the entries expires. Returns True if it is needed to remove it.
               -> IO (IO (),IO ()) -- The tuple (update function, free function)
registerTimerM timerV ttl unreg = if ttl >= 0 then do (index,entry) <- modifyMVar timerV $ (swap <$>) . runStateT (registerTimer' ttl unreg)
                                                      pure (refreshFun index, freeFun index)
                                             else pure $ (pure (), pure ())
        where refreshFun index = void $ runStateMVar timerV (refreshTimerEntry index)
              freeFun index = void $ runStateMVar timerV (freeEntry index)

                                              

{-
unregisterEntry :: (MonadIO m) => Int -> TimerEntry -> TimerT m ()
unregisterEntry index entry = do removePred <- liftIO (timerUnregister entry)
                                 if removePred then freeEntry index 
                                               else refreshTimerEntry index
-}

unregisterEntry :: MVar Timer -> Int -> TimerEntry -> IO ()
unregisterEntry timerV index entry = do 
                                        keepLog TimerLog Normal $ "[timer] entry " ++ show index ++ " has timed out. Calling the callback..."
                                        removePred <- timerUnregister entry
                                        if removePred then runStateMVar timerV (freeEntry index)
                                                      else runStateMVar timerV (refreshTimerEntry index)

freeEntry :: (MonadIO m) => Int -> TimerT m ()
freeEntry index = liftIO (keepLog TimerLog Normal ("[timer] flushing entrie : " ++ show index)) >>
                  freeEntries >> freeIndex
        where freeIndex = modify $ \s -> s{timerCount = index:timerCount s}
              freeEntries = modify $ \s->s{timerMap = M.delete index (timerMap s)}



{-
checkTimeOut :: (MonadIO m) => TimerT m ()
checkTimeOut = do (time,tM) <- (,) <$> liftIO getTime <*> gets timerMap
                  forM_ (M.assocs tM) $ checkTime time
    where checkTime time (index,entry) = when ( (diffUTCTime time $ timerLast entry) >  timerTTL entry) $
                                               unregisterEntry index entry

-}

checkTimeOut :: MVar Timer -> IO ()
checkTimeOut timerV = do (time,tM) <- (,) <$> getTime <*> (timerMap <$> readMVar timerV)
                         forM_ (M.assocs tM) $ checkTime time
                         withMVar timerV $ \v -> keepLog TimerLog Normal $ "[TIMER] " ++ show v
    where checkTime time (index,entry) = when ( (diffUTCTime time $ timerLast entry) >  timerTTL entry) $
                                               unregisterEntry timerV index entry


repeatEach :: MVar Timer -> IO () -> DiffTime -> IO (IO ())
repeatEach timerV act ttl = snd <$> registerTimerM timerV ttl (act >> pure False)


repeatNTimes :: MVar Timer -> IO () -> IO () -> DiffTime -> Int -> IO (IO())
repeatNTimes timerV act end freq ntimes = do initTime <- getTime
                                             let act' = do act
                                                           cur <- getTime
                                                           if diffUTCTime cur initTime >= freq*fromIntegral ntimes
                                                                   then end >> return True
                                                                   else return False
                                             snd <$> registerTimerM timerV freq act'


startTimer :: MVar Timer -> Int -> IO ()
startTimer timer checkfreq = keepLog TimerLog Normal "[timer] cleaning entries..." >>
                             checkTimeOut timer >> keepLog TimerLog Normal "[timer] done" >>
                             threadDelay checkfreq >> startTimer timer checkfreq

instance Binary UTCTime where
    put (UTCTime d h) = put (fromEnum d) >> put (fromEnum h)
    get = UTCTime <$> (toEnum <$> get) <*> (toEnum <$> get)
