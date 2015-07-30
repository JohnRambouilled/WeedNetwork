{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies, GADTs, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Client.Class where

import Control.Monad.State
import Control.Monad.RWS.Lazy
import Control.Monad.Writer
import Control.Concurrent
import Data.Maybe
import Data.Bifunctor
import Data.List
import Data.Tuple
import qualified Data.Map as M

import Log

class (MonadState s m, LogIO m) => SW s m
instance (MonadState s m, LogIO m) => SW s m

type Behaviour a p m r = RWST p Log a m r --p -> StateT a IO [r]
class Modules a p r where onPacket :: Behaviour a p IO r


data MapModule b k p r = MapModule {keyMap :: M.Map k b,
                                    defaultBehaviour :: [MapCallback b k p r]}
                                    
instance Show k => Show (MapModule b k p r) where show = show . M.keys . keyMap

type MapBehaviour b k p r m a = Behaviour (MapModule b k p r) p m a
type MapCallback b k p r = MapBehaviour b k p r IO [r]


class Ord k => MapModules b k p r where
         packetKey :: (MonadIO m) => MapBehaviour b k p r m (Maybe k) --p -> MapModuleT b k p r m (Maybe k)
         entryBehaviour :: b -> [MapCallback b k p r]

instance MapModules b k p r => Modules (MapModule b k p r) p [r] where
        onPacket = do kM <- packetKey 
                      case kM of Nothing -> pure []
                                 Just k -> onKey k
            where onKey k = do bhVL <- fromMaybe <$> gets defaultBehaviour <*> ((runEntryBhv <$>) <$> mapGetEntry k)
                               runBehaviourList bhVL
                  runEntryBhv b = entryBehaviour b
                  runBehaviourList bhVL = concat  <$> sequence bhVL


newMapModule :: [MapCallback b k p r] -> MapModule b k p r
newMapModule defB = MapModule M.empty defB

mapGetEntry :: (MonadState (MapModule b k p r) m, Ord k) => k -> m (Maybe b) --k -> MapModuleT b k p r m (Maybe b)
mapGetEntry k = M.lookup k <$> gets keyMap

removeMapBehaviour :: (MonadState (MapModule b k p r) m, Ord k) =>  k -> m ()
removeMapBehaviour k = modify $ \s -> s{keyMap = M.delete k $ keyMap s}

insertMapBehaviour :: (MonadState (MapModule b k p r) m, Ord k) => k -> b -> m ()
insertMapBehaviour k b = modify $ \s -> s{keyMap = M.insert k b $ keyMap s}

insertMapBehaviourWith :: (MonadState (MapModule b k p r) m, Ord k) => (b -> b -> b) -> k -> b -> m ()
insertMapBehaviourWith f k b = modify $ \s -> s{keyMap = M.insertWith f k b $ keyMap s}


modifyDefaultMapBehaviour :: MonadState (MapModule b k p r) m => ([MapCallback b k p r] -> [MapCallback b k p r]) -> m ()
modifyDefaultMapBehaviour f = modify $ \s -> s{defaultBehaviour = f $ defaultBehaviour s}

{-| Merges a list of mapbehaviours into a single one |-}
joinMapBehaviour :: (MapModules b k p r) => [MapCallback b k p r] -> MapCallback b k p r
joinMapBehaviour l = Prelude.concat <$> sequence l

runRWSTMVar :: MVar a -> p -> RWST p w a IO b -> IO (b, w)
runRWSTMVar aV p aRwst = modifyMVar aV  $ (mkTuple <$>) . (runRWST aRwst $ p)
    where mkTuple (b, a, w) = (a, (b,w))
 
runSWMVar :: LogIO m => MVar a -> RWST () Log a IO b -> m b
runSWMVar aV f = do (b,l) <- liftIO $ runRWSTMVar aV () f
                    tell l >> pure b

modifySWMVar :: LogIO m => MVar a -> (a -> IOLog (a,b)) -> m b
modifySWMVar aV f = do (b,l) <- liftIO . modifyMVar aV $ (mkTuple <$>) . runWriterT . f
                       tell l >> pure b
    where mkTuple ((a,b),l) = (a,(b,l))

modifySWMVar_ :: LogIO m => MVar a -> (a -> IOLog a) -> m ()
modifySWMVar_ aV f = modifySWMVar aV f'
    where f' a = (,) <$> f a <*> pure ()

runStateMVar :: MVar a -> StateT a IO b -> IO b
runStateMVar aV aS = modifyMVar aV $ (swap <$>) . runStateT aS

runModule :: Modules a p r => MVar a -> p -> IO (r, Log)
runModule modV packet = runRWSTMVar modV packet onPacket
        --runStateMVar modV $ onPacket packet

genCallback :: (Modules a p r, Modules b q s) => MVar b
                                             -> Behaviour a p IO [q]
                                             -> ([s] -> Behaviour a p IO r) 
                                             -> Behaviour a p IO r
genCallback bV f g = do lst <- (liftIO . mapM (runModule bV) =<< f)
                        let (rets,logs) = unzip lst 
                        mapM_ tell logs
                        g rets
 

unregisterM :: (MapModules b k p r, LogIO m) => MVar (MapModule b k p r) -> k -> m ()
unregisterM mv key = runSWMVar mv (removeMapBehaviour key)
        


