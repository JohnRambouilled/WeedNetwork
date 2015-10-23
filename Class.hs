{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad




class EventManager a e where emAddHandler :: a -> AddHandler e
                             emFire :: a -> Handler e

data EventEntry e = EventEntry { eFire :: Handler e,
                                 eAddHandler :: AddHandler e,
                                 eFilter :: e -> Bool}
instance EventManager (EventEntry e) e where emAddHandler = eAddHandler
                                             emFire = eFire

type Modifier a = Handler (a -> a)

data ModEvent t a = ModEvent {meLastValue :: Behavior t a, 
                              meChanges :: Event t a,
                              meModifier :: Modifier a}

newModEvent :: Frameworks t => a -> Moment t (ModEvent t a)
newModEvent i = do (e, h) <- newEvent
                   let (e', b) = mapAccum i $ (\f a -> (f a, f a)) <$> e
                   pure $ ModEvent b e' h


applyMod :: (Modifier a -> a -> e -> b) -> ModEvent t a -> Event t e -> Event t b
applyMod f m = apply (f (meModifier m) <$> meLastValue m) 

type ParamMap a k e = M.Map k (a e)
type EventEntryMap k e = ParamMap EventEntry k e

type EventMap k e = ParamMap AddHandler k e 


class (Ord k) => IDable e k where
        extractID :: e -> k

class Mergeable a e where allAddHandlers :: a -> [AddHandler e]
instance Mergeable (EventEntry e) e where allAddHandlers = pure . eAddHandler
instance Mergeable (AddHandler e) e where allAddHandlers = pure
instance Mergeable a e => Mergeable (M.Map k a) e where allAddHandlers = (allAddHandlers =<<) . M.elems

mergeEvents :: (Frameworks t, Mergeable a e) => Event t a -> Moment t (Event t e)
mergeEvents = mergeAddHandlers . (allAddHandlers <$>)

mergeAddHandlers :: Frameworks t => Event t [AddHandler e] -> Moment t (Event t e)
mergeAddHandlers eH = switchE <$> execute (makeAnyMom <$> eH)
    where makeAnyMom :: [AddHandler e] -> FrameworksMoment (AnyMoment Event e)
          makeAnyMom hL = FrameworksMoment $ (trimE . unions =<< mapM fromAddHandler hL)

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


newEventEntry :: (e -> Bool) -> IO (EventEntry e)
newEventEntry f = do (eE,eF) <- newAddHandler
                     pure $ EventEntry eF eE f

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

restrictModEvent :: (a -> b) -> (a -> b -> a) -> ModEvent t a -> ModEvent t b
restrictModEvent f app m = m{meChanges = f <$> meChanges m,
                             meLastValue = f <$> meLastValue m,
                             meModifier = meModifier m . red}
        where red g a = app a $ g $ f a
