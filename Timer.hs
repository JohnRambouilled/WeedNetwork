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

type TimeMap k a = M.Map k (TimeOutEntry, a)



data TimeOutEntry = TimeOutEntry {toeID :: TimeOutID,
                                    toeAction :: IO ()}

lookupTO :: Ord k => k -> TimeMap k a -> Maybe a
lookupTO k m = snd <$> M.lookup k m


insertTOEvent :: (Ord k, Frameworks t ) => Time -> ModEvent t (TimeMap k a) -> Event t (k, a) -> Moment t (Event t (k,a))
insertTOEvent t me e = do (decoE, decoH) <- newEvent
                          reactimate $ applyMod (insertTOWith t (curry decoH)) me e
                          pure decoE

insertTOReactimate :: (Ord k, Frameworks t) => Time -> ModEvent t (TimeMap k a) -> Event t (k, a) -> Moment t ()
insertTOReactimate t me = reactimate . applyMod (insertTO t) me


insertTO :: Ord k => Time -> Modifier (TimeMap k a) -> TimeMap k a -> (k, a) -> IO ()
insertTO t = insertTOWith t $ pure . pure . pure ()

insertTOWith :: Ord k => Time -> (k -> a -> IO ()) -> Modifier (TimeMap k a) -> TimeMap k a -> (k, a) -> IO ()
insertTOWith delay f mod map (k, a) = do e <- newTimeOutEntry delay $ f k a >> (mod $ M.delete k)
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

getTime = getPOSIXTime

