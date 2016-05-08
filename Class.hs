{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import qualified Data.Map as M
import Control.Monad
import Control.Lens
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.List as L

type Log = String

                -- ***** Structures de données *****
-- Les Events sont gérés par des data instance d'EventManager (contenant une AddHandler et un Fire),
-- les Behavior utilisés sont les ModEvents, contenant l'event des changements, et un modifier de la behavior.
{-
data Channel e = Channel {chanEvent :: Event e,
                          chanCloseE :: Event (),
                          chanCloseH :: IO (),
                          chanLogE :: Event Log,
                          chanLogH :: Handler Log}
-}
newtype Channel e = Channel{runChannel :: TChan (Either () e)}
instance Functor Channel where fmap f (Channel e) = Channel $ fmap f <$> e

--type ChannelEntry e = (Channel e, Handler e) 

newChannel :: STM (Channel e)
newChannel = Channel <$> newTChan

fireChannel :: Channel e -> e -> IO ()
fireChannel c = atomically . writeTChan (runChannel c) . Right

closeChannel :: Channel e -> IO ()
closeChannel c = atomically . writeTChan (runChannel c) $ Left ()

register :: Channel e -> (e -> IO a) -> IO (Channel a)
register c f = do c' <- newTChanIO
                  forkIO $ loop c'
                  pure c'
    where loop c' = do e <- atomically $ readTChan (runChannel c)
                       case e of Left () -> closeChannel c'
                                 Right a -> fireChannel c' $ f a

register_ :: Channel e -> (e -> IO ()) -> IO ()
register_ c f = do forkIO $ loop
                    pure ()
     where loop = do r <- atomically $ readTChan (runChannel c)
                     case r of Left () -> pure ()
                               Right e -> f e >> loop


registerClose :: Channel e -> IO () -> IO ()
registerClose c a = void $ forkIO loop
        where loop = do e <- atomically $ readTChan (runChannel c)
                        case e of Left () -> a
                                  Right _ -> loop






data BehaviorC a = BehaviorC {bcLastValue :: Behavior a, 
                                bcChanges :: Event a}
instance Functor BehaviorC where fmap f b = BehaviorC (f <$> bcLastValue b) (f <$> bcChanges b)

type Modifier a = Handler (a -> a)
data BehaviorMod a = BehaviorMod {bmBhvC :: BehaviorC a,
                                  bmModifier :: Modifier a}
bmLastValue = bcLastValue . bmBhvC
bmChanges = bcChanges . bmBhvC


buildCloseHandle :: (Ord k, Closable a) => Behavior (M.Map k a) -> MomentIO (k -> IO ())
buildCloseHandle mapB = do (clE, clH) <- newEvent
                           reactimate $ apply (close <$> mapB) clE
                           pure clH
        where close map k = case k `M.lookup` map of
                                Nothing -> pure ()
                                Just ec -> closeH ec 


type ParamMap a k e = M.Map k (a e)  -- ^ syntax-helper pour les types paramétriques
type EventMap k e = ParamMap Event k e       -- ^ AddHandler map : version sans fire
type AddHMap k e = ParamMap AddHandler k e
type ChannelMap k e = ParamMap Channel k e
type ChannelEntryMap k e = (M.Map k (ChannelEntry e))

-- | Class de type des data contenant une clef d'identification
class (Ord k) => IDable e k where
        extractID :: e -> k

                -- ***** Switch et Merges *****
-- Mergeable définit la classe des structures permettant d'extraire un event (éventuellement compilé).
-- mergeEvents renvoi un event égal au dernier event total reçu.

-- | Extraction d'un event-compilé d'une data. (par exemple l'union de tout les event d'une map)
class Mergeable a e where allEvents :: a -> Event [e]
instance Mergeable (Event e) e where allEvents = fmap pure 
instance Mergeable (Channel e) e where allEvents = (pure <$>) . chanEvent
instance Mergeable a e => Mergeable (M.Map k a) e where allEvents = foldr (unionWith $ pure id) never . (allEvents <$>) . M.elems

switchEE :: (MonadMoment m, Mergeable a e) => Event a -> m (Event [e])
switchEE = switchE . (allEvents <$>)

spillEvent :: Event [a] -> MomentIO (Event a)
spillEvent e = do (e',h) <- newEvent
                  reactimate $ mapM_ h <$> e
                  pure e'

mergeEvents :: Mergeable a e => Event a -> MomentIO (Event e)
mergeEvents e = spillEvent =<< switchEE e

unionM :: [Event a] -> MomentIO (Event a)
unionM = spillEvent . unionL

unionL :: [Event a] -> Event [a]
unionL eL = ($ []) <$> unions (((\a s -> a : s) <$>) <$> eL)

switchBC :: MonadMoment m => Behavior a -> BehaviorC (BehaviorC a) -> m (BehaviorC a)
switchBC i bc = BehaviorC <$> switchB i (bcLastValue <$> e)
                          <*> switchE (bcChanges <$> e)
    where e = bcChanges bc


        -- ***** Fonctions utiles *****
        
-- | apply adapté aux ModEvent : les arguments du premier parametre sont :
--      modifieur du ModEvent
--      dernière valeur du ModEvent
--      valeur de l'Event
applyMod :: (Modifier a -> a -> e -> b) -> BehaviorMod a -> Event e -> Event b
applyMod f m = apply (f (bmModifier m) <$> bmLastValue m) 

fireKeyBhv :: (IDable e k) =>  Behavior (M.Map k (Handler e)) -> Event e -> MomentIO ()
fireKeyBhv bhv e = reactimate . filterJust $ (f <$> bhv) <@> e
    where f map e = fireKey map e

fireKeyWith :: (IDable e k) => (a -> Handler e) -> M.Map k a -> e -> Maybe (IO ())
fireKeyWith acc m e =  (\a -> acc a $ e) <$> (extractID e) `M.lookup` m

fireKey :: (IDable e k) => M.Map k (Handler e) -> e -> Maybe (IO ())
fireKey m e = ($e) <$> (extractID e) `M.lookup` m 

-- necessite une fonction a -> b de restriction (exctraction de la sous-partie)
-- et une fonction a -> b -> a de remplacement de la sous partie dans l'ensemble
restrictBehaviorMod :: (a -> b) -> (a -> b -> a) -> BehaviorMod a -> BehaviorMod b
restrictBehaviorMod f app m = m{bmBhvC = f <$> bmBhvC m,
                                bmModifier = bmModifier m . red}
        where red g a = app a $ g $ f a


linkClosable :: (Closable a, Closable b) => a -> b -> MomentIO ()
linkClosable a b = do reactimate $ closeH a <$ closeE b
                      reactimate $ closeH b <$ closeE a

                -- ***** Spliting the Events *****

splitEvent :: (e -> Bool) -> Event e -> (Event e, Event e)
splitEvent f eE = split $ mkEither <$> eE
    where mkEither e = if f e then Right e else Left e

filterChan :: (e -> Bool) -> Channel e -> Channel e
filterChan f e = e{chanEvent = filterE f $ chanEvent e}

 
                -- ***** Constructeurs *****
newChannel :: MomentIO (ChannelEntry e)
newChannel = do ((e, h), (ce,ch), (le, lh)) <- (,,) <$> newEvent <*> newEvent <*> newEvent
                return (Channel e ce (ch ()) le lh, h)


buildCloseListWith :: (Closable e, Eq a) => Event (a, e) -> MomentIO (BehaviorMod [a])
buildCloseListWith = buildCloseData (:) L.delete [] . fmap mkInput
    where mkInput (a, c) = (a, a, c)

buildCloseMap :: (Closable e, Ord k) => Event (k, e) -> MomentIO (BehaviorMod (M.Map k e))
buildCloseMap = buildCloseMapWith . ((\(k,e) -> ((k,e), e)) <$>)

buildCloseMapWith :: (Closable e, Ord k) => Event ((k, e), a) -> MomentIO (BehaviorMod (M.Map k a))
buildCloseMapWith = buildCloseData (uncurry M.insert) M.delete M.empty . fmap mkInput
    where mkInput ((k, c), a) = ((k,a), k, c)

buildCloseData :: Closable e => (a -> b -> b) -> (x -> b -> b) -> b -> Event (a, x, e) -> MomentIO (BehaviorMod b)
buildCloseData insert delete start event = do d <- newBehaviorMod start
                                              reactimate =<< execute (applyMod insert' d event)
                                              pure d
            where insert' mod _ (a, x, chan) = do reactimate $ pure (mod $ delete x) <$> closeE chan
                                                  pure (mod $ insert a)
-- | Prend une valeur initiale, et construit un nouveau ModEvent
newBehaviorMod :: a -> MomentIO (BehaviorMod a)
newBehaviorMod i = do (e, h) <- newEvent
                      (e', b) <- mapAccum i $ (\f a -> (f a, f a)) <$> e
                      pure $ BehaviorMod (BehaviorC b e') h

liftIOEvent :: Event (IO a) -> MomentIO (Event a)
liftIOEvent = execute . (liftIO <$>)


deleteLookup :: Ord k => k -> M.Map k a -> Maybe (a, M.Map k a)
deleteLookup k m = case M.updateLookupWithKey f k m of
                    (Just a, m) -> Just (a,m)
                    (Nothing, _) -> Nothing
    where f _ _ = Nothing

ioPutS :: MonadIO m => String -> m ()
ioPutS = liftIO . Prelude.putStrLn
