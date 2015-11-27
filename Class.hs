{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad

                -- ***** Structures de données *****
-- Les Events sont gérés par des data instance d'EventManager (contenant une AddHandler et un Fire),
-- les Behavior utilisés sont les ModEvents, contenant l'event des changements, et un modifier de la behavior.

    -- | Class de type des structures contenant un event
class EventManager a e where emAddHandler :: a -> AddHandler e
                             emFire :: a -> Handler e

data EventC e = EventC {ceAddHandler :: AddHandler e,
                        ceClose :: Handler (),
                        ceCloseEvent :: AddHandler ()}
instance Functor EventC where fmap f e = e{ceAddHandler = f <$> ceAddHandler e}


data EventEntry e = EventEntry {eFire :: Handler e,
                                eEventC :: EventC e}
eAddHandler = ceAddHandler . eEventC
eClose = ceClose . eEventC
eCloseEvent = ceCloseEvent . eEventC

instance EventManager (EventEntry e) e where emAddHandler = ceAddHandler . eEventC 
                                             emFire = eFire

data BehaviorC t a = BehaviorC {bcLastValue :: Behavior t a, 
                                bcChanges :: Event t a}
instance Functor (BehaviorC t) where fmap f b = BehaviorC (f <$> bcLastValue b) (f <$> bcChanges b)

type Modifier a = Handler (a -> a)
data BehaviorMod t a = BehaviorMod {bmBhvC :: BehaviorC t a,
                                    bmModifier :: Modifier a}
bmLastValue = bcLastValue . bmBhvC
bmChanges = bcChanges . bmBhvC


buildCloseHandle :: (Ord k, Frameworks t) => Behavior t (EventCMap k e) -> Moment t (k -> IO ())
buildCloseHandle mapB = do (closeE, closeH) <- newEvent
                           reactimate $ apply (close <$> mapB) closeE
                           pure closeH
        where close map k = case k `M.lookup` map of
                                Nothing -> pure ()
                                Just ec -> ceClose ec $ ()


type ParamMap a k e = M.Map k (a e)  -- ^ syntax-helper pour les types paramétriques
type EventEntryMap k e = ParamMap EventEntry k e  -- ^ Map k (EventEntry e) : map d'eventEntry
type EventMap k e = ParamMap AddHandler k e       -- ^ AddHandler map : version sans fire

type EventCMap k e = M.Map k (EventC e)


-- | Class de type des data contenant une clef d'identification
class (Ord k) => IDable e k where
        extractID :: e -> k


                -- ***** Switch et Merges *****
-- Mergeable définit la classe des structures permettant d'extraire un event (éventuellement compilé).
-- mergeEvents renvoi un event égal au dernier event total reçu.

-- | Extraction d'un event-compilé d'une data. (par exemple l'union de tout les event d'une map)
class Mergeable a e where allAddHandlers :: a -> AddHandler e
instance Mergeable (EventC e) e where allAddHandlers = ceAddHandler
instance Mergeable (EventEntry e) e where allAddHandlers = ceAddHandler . eEventC
instance Mergeable (AddHandler e) e where allAddHandlers = id
instance Mergeable a e => Mergeable (M.Map k a) e where allAddHandlers = mergeAddHandlersList . (allAddHandlers <$>) . M.elems


 
-- | Switch : a chaque nouvelle valeur a, l'event-compilé de a est utilisé comme nouvel event.
mergeEvents :: (Frameworks t, Mergeable a e) => Event t a -> Moment t (Event t e)
mergeEvents = mergeAddHandlers . (allAddHandlers <$>)


-- | a chaque nouvelle AddHandler, unregister la précédente, puis
-- register la handler de l'event-switch, et push le unregister. => Switch
mergeAddHandlers :: Frameworks t => Event t (AddHandler e) -> Moment t (Event t e)
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



        -- ***** Fonctions utiles *****
        
-- | apply adapté aux ModEvent : les arguments du premier parametre sont :
--      modifieur du ModEvent
--      dernière valeur du ModEvent
--      valeur de l'Event
applyMod :: (Modifier a -> a -> e -> b) -> BehaviorMod t a -> Event t e -> Event t b
applyMod f m = apply (f (bmModifier m) <$> bcLastValue (bmBhvC m)) 

fireKeyBhv :: (Frameworks t, IDable e k, EventManager a e) =>  Behavior t (M.Map k a) -> Event t e -> Moment t ()
fireKeyBhv bhv e = reactimate . filterJust $ (f <$> bhv) <@> e
    where f map e = fireKey map e

-- | Close if predicate is True
closeOnEC :: (e -> Bool) -> EventC e -> IO (IO ()) 
closeOnEC f ec = ceAddHandler ec `register` cl
    where cl e = if f e then ceClose ec $ () else pure ()

{-| Fires the value on the specified key event |-}
fireKey :: (IDable e k, EventManager a e) => M.Map k a -> e -> Maybe (IO ())
fireKey m e = fireKey' m (extractID e) e

fireKeyWith :: (IDable e k) => (a -> Handler e) -> M.Map k a -> e -> Maybe (IO ())
fireKeyWith acc m e =  (\a -> acc a $ e) <$> (extractID e) `M.lookup` m

fireKey' :: (Ord k, EventManager a e) => M.Map k a -> k -> e -> Maybe (IO ())
fireKey' m k e = f <$> k `M.lookup` m
  where f entry = emFire entry e

{-| Extracts the event stream from a given key |-}
kEvents :: (Ord k, EventManager a e) => M.Map k a -> k -> Maybe (AddHandler e)
kEvents m k = emAddHandler <$> k `M.lookup` m

-- | permet de restreindre un ModEvent a une sous-partie : 
-- necessite une fonction a -> b de restriction (exctraction de la sous-partie)
-- et une fonction a -> b -> a de remplacement de la sous partie dans l'ensemble
restrictBehaviorMod :: (a -> b) -> (a -> b -> a) -> BehaviorMod t a -> BehaviorMod t b
restrictBehaviorMod f app m = m{bmBhvC = f <$> bmBhvC m,
                                bmModifier = bmModifier m . red}
        where red g a = app a $ g $ f a


                -- ***** Spliting the Events *****

splitEvent :: Frameworks t => (e -> Bool) -> Event t e -> Moment t (Event t e, Event t e)
splitEvent f eE = do ((failE, failH), (passE, passH)) <- (,) <$> newEvent <*> newEvent
                     reactimate  $ (\e -> if f e then passH e else failH e) <$> eE
                     pure (failE, passE)

splitEither :: Frameworks t => Event t (Either a b) -> Moment t (Event t a, Event t b)
splitEither e = do ((leftE, leftH), (rightE, rightH)) <- (,) <$> newEvent <*> newEvent
                   let f (Left a) = leftH a
                       f (Right a) = rightH a
                   reactimate $ f <$> e
                   pure (leftE, rightE)



                -- ***** Constructeurs *****

buildEventCMap ::  (Frameworks t, Ord k) => Event t (k, EventC e) -> Moment t (BehaviorMod t (EventCMap k e))
buildEventCMap = buildEventCMapWith . ((\(k,e) -> ((k,e), e)) <$>)

buildEventCMapWith :: (Frameworks t, Ord k) => Event t ((k, EventC e), a) -> Moment t (BehaviorMod t (M.Map k a))
buildEventCMapWith e = do map <- newBehaviorMod M.empty
                          reactimate $ applyMod insert map e
                          pure map
    where insert mod map ((k,ce),a) = do mod $ M.insert k a
                                         ceCloseEvent ce `register` pure (mod $ M.delete k)
                                         pure ()

newEventC :: IO (Handler e, EventC e)
newEventC = do (eE, eF) <- newAddHandler
               (cE, cF) <- newAddHandler
               pure (eF, EventC eE cF cE)

newEventEntry :: IO (EventEntry e)
newEventEntry = do (eH,eC) <- newEventC
                   pure $ EventEntry eH eC

-- | Prend une valeur initiale, et construit un nouveau ModEvent
newBehaviorMod :: Frameworks t => a -> Moment t (BehaviorMod t a)
newBehaviorMod i = do (e, h) <- newEvent
                      let (e', b) = mapAccum i $ (\f a -> (f a, f a)) <$> e
                      pure $ BehaviorMod (BehaviorC b e') h

liftIOEvent :: Frameworks t => Event t (IO a) -> Moment t (Event t a)
liftIOEvent e = do (e', h) <- newEvent
                   reactimate $ (h =<<) <$> e
                   pure e'

-- | DO NOT USE : ça a l'air cool, mais ça leak! Mais je sais pas pourquoi, alors je laisse ça la
mergeAddHandlersLeak :: Frameworks t => Event t [AddHandler e] -> Moment t (Event t e)
mergeAddHandlersLeak eH = switchE <$> execute (makeAnyMom <$> eH)
    where makeAnyMom :: [AddHandler e] -> FrameworksMoment (AnyMoment Event e)
          makeAnyMom hL = FrameworksMoment $ (trimE . unions =<< mapM fromAddHandler hL)

