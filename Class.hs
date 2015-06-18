{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies, GADTs, FlexibleContexts, FlexibleInstances #-}

module Class where

import Control.Monad.State
import Control.Concurrent
import Data.Maybe
import Data.List
import Data.Tuple
import qualified Data.Map as M


type Behaviour a p r = p -> StateT a IO [r]
class Modules a p r where onPacket :: Behaviour a p r


data MapModule b k p r = MapModule {keyMap :: M.Map k b,
                                    defaultBehaviour :: [MapBehaviour b k p r]}
                                    
instance Show k => Show (MapModule b k p r) where show = show . M.keys . keyMap

type MapBehaviour b k p r = Behaviour (MapModule b k p r) p r
        
type MapModuleT b k p r = StateT (MapModule b k p r)

class Ord k => MapModules b k p r where
         packetKey :: MonadIO m => p -> MapModuleT b k p r m (Maybe k)
         entryBehaviour :: b -> [b -> MapBehaviour b k p r]

instance MapModules b k p r => Modules (MapModule b k p r) p r where
        onPacket p = join (maybe (return []) onKey <$> packetKey p)
            where onKey k = do bhVL <- fromMaybe <$> gets defaultBehaviour <*> ((runEntryBhv <$>) <$> mapGetEntry k)
                               runBehaviourList bhVL
                  runEntryBhv b = map ($b) $ entryBehaviour b
                  runBehaviourList bhVL = concat  <$> (sequence $ map ($p) bhVL)


newMapModule :: [MapBehaviour b k p r] -> MapModule b k p r
newMapModule defB = MapModule M.empty defB

mapGetEntry :: (Monad m, Ord k) => k -> MapModuleT b k p r m (Maybe b)
mapGetEntry k = M.lookup k <$> gets keyMap

removeMapBehaviour :: (Monad m, Ord k) => k -> MapModuleT b k p r m ()
removeMapBehaviour k = modify $ \s -> s{keyMap = M.delete k $ keyMap s}

insertMapBehaviour :: (Monad m, Ord k) => k -> b -> MapModuleT b k p r m ()
insertMapBehaviour k b = modify $ \s -> s{keyMap = M.insert k b $ keyMap s}

insertMapBehaviourWith :: (Monad m, Ord k) => (b -> b -> b) -> k -> b -> MapModuleT b k p r m ()
insertMapBehaviourWith f k b = modify $ \s -> s{keyMap = M.insertWith f k b $ keyMap s}


modifyDefaultMapBehaviour :: Monad m => ([MapBehaviour b k p r] -> [MapBehaviour b k p r]) -> MapModuleT b k p r m ()
modifyDefaultMapBehaviour f = modify $ \s -> s{defaultBehaviour = f $ defaultBehaviour s}

{-| Merges a list of mapbehaviours into a single one |-}
joinMapBehaviour :: (MapModules b k p r) => [MapBehaviour b k p r] -> MapBehaviour b k p r
joinMapBehaviour l pkt = Prelude.concat <$> mapM ($pkt) l

runStateMVar :: MVar a -> StateT a IO b -> IO b
runStateMVar aV aS = modifyMVar aV  $ (swap <$>) . runStateT aS

runModule :: Modules a p r => MVar a -> p -> IO [r]
runModule modV packet = runStateMVar modV $ onPacket packet

genCallback :: (Modules a p r, Modules b q s) => MVar b 
                                             -> Behaviour a p q 
                                             -> (p -> Behaviour a s r) 
                                             -> Behaviour a p r
genCallback bV f g p = do
       qL <- f p
       sL <- concat <$> sequence (map (liftIO . runModule bV) qL)
       concat <$> (sequence $ map (g p) sL)
 

unregisterM :: (MapModules b k p r) => MVar (MapModule b k p r) -> k -> IO ()
unregisterM mv key = modifyMVar_ mv $ \v -> snd <$> runStateT (removeMapBehaviour key) v



