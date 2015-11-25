module Timer where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Map as M

import Class

type TimeOutID = ThreadId
type Time = POSIXTime

data TimeOutEntry = TimeOutEntry {toeID :: TimeOutID,
                                    toeAction :: IO ()}


buildTimeOut :: (Frameworks t, Ord k) => Time -> Event t (k, EventC e) -> Event t k -> Moment t ()
buildTimeOut t addE refE = do buildE <- liftIOEvent $ genTime <$> addE
                              toMap <- buildEventCMapWith buildE
                              reactimate $ applyMod refresh toMap refE
    where genTime (k,ce) = do toE <- newTimeOutEntry t $ ceClose ce $ ()
                              pure ((k,ce), toE)
          refresh mod toMap k = case k `M.lookup` toMap of
                                  Nothing -> pure ()
                                  Just toE -> mod . M.insert k =<< refreshTimeOutEntry toE


newRepeater :: Maybe Int -> Time -> IO () -> IO TimeOutEntry
newRepeater nM t a = let action = (case nM of
                                    Just n -> replicateM_ n
                                    Nothing -> forever) $ waitFor t >> a 
                    in do a
                          TimeOutEntry <$> forkIO action <*> pure action

newTimeOutEntry ::  Time -> IO () -> IO TimeOutEntry 
newTimeOutEntry delay close = do let action = waitFor delay >> close
                                 id <- forkIO action
                                 pure $ TimeOutEntry id action 

refreshTimeOutEntry :: TimeOutEntry -> IO TimeOutEntry
refreshTimeOutEntry e = do killThread $ toeID e
                           id <- forkIO $ toeAction e
                           pure e{toeID = id}

killTimeOut :: TimeOutEntry -> IO ()
killTimeOut = killThread . toeID

waitFor :: Time -> IO ()
waitFor = threadDelay . round . (*10^6)

getTime :: MonadIO m => m Time
getTime = liftIO $ getPOSIXTime

{-
insertTOEvent :: (Ord k, Frameworks t ) => Time -> ModEvent t (TimeMap k a) -> Event t (k, a) -> Moment t (Event t (k,a))
insertTOEvent t me e = do (decoE, decoH) <- newEvent
                          reactimate $ applyMod (insertTOWith t (flip $ curry decoH)) me e
                          pure decoE

insertTOReactimateWithTime :: (Ord k, Frameworks t) => (a -> k -> IO ()) -> ModEvent t (TimeMap k a) -> Event t (Time, (k,a)) -> Moment t ()
insertTOReactimateWithTime f me = reactimate . applyMod insertWithTime me
    where insertWithTime mod m (t,(k,a)) = insertTOWith t f mod m (k,a)

insertTOReactimate :: (Ord k, Frameworks t) => Time -> (a -> k -> IO ()) -> ModEvent t (TimeMap k a) -> Event t (k, a) -> Moment t ()
insertTOReactimate t f me = reactimate . applyMod (insertTOWith t f) me

insertTOReactimate_ :: (Ord k, Frameworks t) => Time -> ModEvent t (TimeMap k a) -> Event t (k, a) -> Moment t ()
insertTOReactimate_ t = insertTOReactimate t $ pure . pure . pure ()

insertTO :: Ord k => Time -> Modifier (TimeMap k a) -> TimeMap k a -> (k, a) -> IO ()
insertTO t = insertTOWith t $ pure . pure . pure ()

insertTOWith :: Ord k => Time -> (a -> k -> IO ()) -> Modifier (TimeMap k a) -> TimeMap k a -> (k, a) -> IO ()
insertTOWith delay f mod map (k, a) = do e <- newTimeOutEntry delay $ f a k >> (mod $ M.delete k)
                                         case k `M.lookup` map of Nothing -> pure ()
                                                                  Just e -> killTimeOut $ fst e
                                         mod $ M.insert k (e,a)

deleteTOReactimate :: (Ord k, Frameworks t) => ModEvent t (TimeMap k a) -> Event t k -> Moment t ()
deleteTOReactimate me = reactimate . applyMod deleteTO me

deleteTO :: Ord k => Modifier (TimeMap k a) -> TimeMap k a -> k -> IO ()
deleteTO mod map k = case k `M.lookup` map of
                        Nothing -> pure ()
                        Just e -> do killTimeOut $ fst e
                                     mod $ M.delete k
-}
