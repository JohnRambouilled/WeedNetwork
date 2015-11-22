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

    -- | data minimale d'Event
data EventEntry e = EventEntry { eFire :: Handler e,
                                 eAddHandler :: AddHandler e}
instance EventManager (EventEntry e) e where emAddHandler = eAddHandler
                                             emFire = eFire

    -- | Behavior container : LastValue (Behavior a)
    --                        Changes (Event a)
    --                        Modifier ((a -> a) -> IO ())
    --  Derniere valeur, event des modifications, et modifieur.
type Modifier a = Handler (a -> a)
data ModEvent t a = ModEvent {meLastValue :: Behavior t a, 
                              meChanges :: Event t a,
                              meModifier :: Modifier a}

type ParamMap a k e = M.Map k (a e)  -- ^ syntax-helper pour les types paramétriques
type EventEntryMap k e = ParamMap EventEntry k e  -- ^ Map k (EventEntry e) : map d'eventEntry
type EventMap k e = ParamMap AddHandler k e       -- ^ AddHandler map : version sans fire



-- | Class de type des data contenant une clef d'identification
class (Ord k) => IDable e k where
        extractID :: e -> k


                -- ***** Switch et Merges *****
-- Mergeable définit la classe des structures permettant d'extraire un event (éventuellement compilé).
-- mergeEvents renvoi un event égal au dernier event total reçu.

-- | Extraction d'un event-compilé d'une data. (par exemple l'union de tout les event d'une map)
class Mergeable a e where allAddHandlers :: a -> AddHandler e
instance Mergeable (EventEntry e) e where allAddHandlers = eAddHandler
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
applyMod :: (Modifier a -> a -> e -> b) -> ModEvent t a -> Event t e -> Event t b
applyMod f m = apply (f (meModifier m) <$> meLastValue m) 

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
restrictModEvent :: (a -> b) -> (a -> b -> a) -> ModEvent t a -> ModEvent t b
restrictModEvent f app m = m{meChanges = f <$> meChanges m,
                             meLastValue = f <$> meLastValue m,
                             meModifier = meModifier m . red}
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
newEventEntry :: IO (EventEntry e)
newEventEntry = do (eE,eF) <- newAddHandler
                   pure $ EventEntry eF eE

-- | Prend une valeur initiale, et construit un nouveau ModEvent
newModEvent :: Frameworks t => a -> Moment t (ModEvent t a)
newModEvent i = do (e, h) <- newEvent
                   let (e', b) = mapAccum i $ (\f a -> (f a, f a)) <$> e
                   pure $ ModEvent b e' h



-- | DO NOT USE : ça a l'air cool, mais ça leak! Mais je sais pas pourquoi, alors je laisse ça la
mergeAddHandlersLeak :: Frameworks t => Event t [AddHandler e] -> Moment t (Event t e)
mergeAddHandlersLeak eH = switchE <$> execute (makeAnyMom <$> eH)
    where makeAnyMom :: [AddHandler e] -> FrameworksMoment (AnyMoment Event e)
          makeAnyMom hL = FrameworksMoment $ (trimE . unions =<< mapM fromAddHandler hL)

