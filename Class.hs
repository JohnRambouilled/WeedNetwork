{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import Control.Lens

                -- ***** Structures de données *****
-- Les Events sont gérés par des data instance d'EventManager (contenant une AddHandler et un Fire),
-- les Behavior utilisés sont les ModEvents, contenant l'event des changements, et un modifier de la behavior.

    -- | Class de type des structures contenant un event
class EventManager a e where emEvent :: a -> Event e
                             emFire :: a -> Handler e

type CloseEvent = (Event (), IO ())
newCloseEvent = over _2 ($ ()) <$> newEvent :: MomentIO CloseEvent

data EventC e = EventC {ceEvent :: Event e,
                        ceClose :: IO (),
                        ceCloseEvent :: Event ()}
instance Functor EventC where fmap f e = e{ceEvent = f <$> ceEvent e}


data EventEntry e = EventEntry {eFire :: Handler e,
                                eEventC :: EventC e}
eEvent = ceEvent . eEventC
eClose = ceClose . eEventC
eCloseEvent = ceCloseEvent . eEventC

instance EventManager (EventEntry e) e where emEvent = ceEvent . eEventC 
                                             emFire = eFire

data BehaviorC a = BehaviorC {bcLastValue :: Behavior a, 
                                bcChanges :: Event a}
instance Functor BehaviorC where fmap f b = BehaviorC (f <$> bcLastValue b) (f <$> bcChanges b)

type Modifier a = Handler (a -> a)
data BehaviorMod a = BehaviorMod {bmBhvC :: BehaviorC a,
                                  bmModifier :: Modifier a}
bmLastValue = bcLastValue . bmBhvC
bmChanges = bcChanges . bmBhvC


buildCloseHandle :: Ord k => Behavior (EventCMap k e) -> MomentIO (k -> IO ())
buildCloseHandle mapB = do (closeE, closeH) <- newEvent
                           reactimate $ apply (close <$> mapB) closeE
                           pure closeH
        where close map k = case k `M.lookup` map of
                                Nothing -> pure ()
                                Just ec -> ceClose ec 


type ParamMap a k e = M.Map k (a e)  -- ^ syntax-helper pour les types paramétriques
type EventEntryMap k e = ParamMap EventEntry k e  -- ^ Map k (EventEntry e) : map d'eventEntry
type EventMap k e = ParamMap Event k e       -- ^ AddHandler map : version sans fire
type AddHMap k e = ParamMap AddHandler k e
type EventCMap k e = ParamMap EventC k e 


-- | Class de type des data contenant une clef d'identification
class (Ord k) => IDable e k where
        extractID :: e -> k
--instance Ord k => IDable k k where extractID = id

                -- ***** Switch et Merges *****
-- Mergeable définit la classe des structures permettant d'extraire un event (éventuellement compilé).
-- mergeEvents renvoi un event égal au dernier event total reçu.

-- | Extraction d'un event-compilé d'une data. (par exemple l'union de tout les event d'une map)
class Mergeable a e where allEvents :: a -> Event [e]
instance Mergeable (Event e) e where allEvents = fmap pure 
instance Mergeable (EventC e) e where allEvents = (pure <$>) . ceEvent
instance Mergeable (EventEntry e) e where allEvents = (pure <$>) . ceEvent . eEventC
--instance Mergeable (AddHandler e) e where allEvents = id
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

fireKeyBhv :: (IDable e k, EventManager a e) =>  Behavior (M.Map k a) -> Event e -> MomentIO ()
fireKeyBhv bhv e = reactimate . filterJust $ (f <$> bhv) <@> e
    where f map e = fireKey map e

-- | Close if predicate is True
closeOnEC :: (e -> Bool) -> EventC e -> MomentIO () 
closeOnEC f ec = reactimate $ cl <$> ceEvent ec
    where cl e = if f e then ceClose ec else pure ()

{-| Fires the value on the specified key event |-}
fireKey :: (IDable e k, EventManager a e) => M.Map k a -> e -> Maybe (IO ())
fireKey m e = fireKey' m (extractID e) e

fireKeyWith :: (IDable e k) => (a -> Handler e) -> M.Map k a -> e -> Maybe (IO ())
fireKeyWith acc m e =  (\a -> acc a $ e) <$> (extractID e) `M.lookup` m

fireKey' :: (Ord k, EventManager a e) => M.Map k a -> k -> e -> Maybe (IO ())
fireKey' m k e = f <$> k `M.lookup` m
  where f entry = emFire entry e

{-| Extracts the event stream from a given key |-}
kEvents :: (Ord k, EventManager a e) => M.Map k a -> k -> Maybe (Event e)
kEvents m k = emEvent <$> k `M.lookup` m

-- | permet de restreindre un ModEvent a une sous-partie : 
-- necessite une fonction a -> b de restriction (exctraction de la sous-partie)
-- et une fonction a -> b -> a de remplacement de la sous partie dans l'ensemble
restrictBehaviorMod :: (a -> b) -> (a -> b -> a) -> BehaviorMod a -> BehaviorMod b
restrictBehaviorMod f app m = m{bmBhvC = f <$> bmBhvC m,
                                bmModifier = bmModifier m . red}
        where red g a = app a $ g $ f a


                -- ***** Spliting the Events *****

splitEvent :: (e -> Bool) -> Event e -> (Event e, Event e)
splitEvent f eE = split $ mkEither <$> eE
    where mkEither e = if f e then Right e else Left e


                -- ***** Constructeurs *****

buildEventCMap ::  Ord k => Event (k, EventC e) -> MomentIO (BehaviorMod (EventCMap k e))
buildEventCMap = buildEventCMapWith . ((\(k,e) -> ((k,e), e)) <$>)

buildEventCMapWith :: Ord k => Event ((k, EventC e), a) -> MomentIO (BehaviorMod (M.Map k a))
buildEventCMapWith e = do map <- newBehaviorMod M.empty
                          reactimate =<< (execute $ applyMod insert map e)  -- TO CKECK
                          pure map
    where insert :: Ord k => Modifier (M.Map k a) -> M.Map k a -> ((k, EventC e), a) -> MomentIO (IO ())
          insert mod map ((k,ce),a) = do 
                                         reactimate $ pure (mod $ M.delete k) <$> ceCloseEvent ce
                                         pure (mod $ M.insert k a)


newEventC :: MomentIO (Handler e, EventC e)
newEventC = do (eE, eF) <- newEvent
               (cE, cF) <- newEvent
               pure (eF, EventC eE (cF $ ()) cE)

newEventEntry :: MomentIO (EventEntry e)
newEventEntry = do (eH,eC) <- newEventC
                   pure $ EventEntry eH eC

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
    --do (e', h) <- newEvent
                  -- reactimate $ (h =<<) <$> e
                  -- pure e'

{-
-- | DO NOT USE : ça a l'air cool, mais ça leak! Mais je sais pas pourquoi, alors je laisse ça la
mergeAddHandlersLeak :: Event [AddHandler e] -> Moment (Event e)
mergeAddHandlersLeak eH = switchE <$> execute (makeAnyMom <$> eH)
    where makeAnyMom :: [AddHandler e] -> FrameworksMoment (AnyMoment Event e)
          makeAnyMom hL = FrameworksMoment $ (trimE . unions =<< mapM fromAddHandler hL)
-- | a chaque nouvelle AddHandler, unregister la précédente, puis
-- register la handler de l'event-switch, et push le unregister. => Switch
mergeAddHandlers :: Event (AddHandler e) -> Moment (Event e)
mergeAddHandlers eAH = do (e,h) <-  newEvent
                          (unregE, unregH) <- newEvent
                          reactimate $ reg h unregH <$> eAH
                          reactimate $ stepper (pure ()) unregE <@ eAH
                          pure e
        where reg :: Handler e -> Handler (IO ()) -> AddHandler e -> IO ()
              reg h urH adH = adH `register` h >>= urH 

-- | Merge addHandler
mergeAddHandlersList :: [AddHandler e] -> AddHandler e
mergeAddHandlersList l = AddHandler $ \h -> sequence_ <$> forM l (`register` h)
-}
