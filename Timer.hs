module Timer where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Concurrent
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


insertTOReactimate :: (Ord k, Frameworks t) => Time -> ModEvent t (TimeMap k a) -> Event t (k, a) -> Moment t ()
insertTOReactimate t me = reactimate . applyMod (insertTO t) me

insertTO :: Ord k => Time -> Modifier (TimeMap k a) -> TimeMap k a -> (k, a) -> IO ()
insertTO delay mod map (k, a) = do e <- newTimeOutEntry delay $ mod $ M.delete k
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


newTimeOutEntry ::  Time -> IO () -> IO TimeOutEntry 
newTimeOutEntry delay close = do let action = (threadDelay . round $ delay * (10^6)) >> close
                                 id <- forkIO action
                                 pure $ TimeOutEntry id action 

refreshTimeOutEntry :: TimeOutEntry -> IO TimeOutEntry
refreshTimeOutEntry e = do killThread $ toeID e
                           id <- forkIO $ toeAction e
                           pure e{toeID = id}

killTimeOut :: TimeOutEntry -> IO ()
killTimeOut = killThread . toeID




