{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad



--newtype Handle a = Handle {fire :: Handler a}

data EventEntry e = EventEntry { eFire :: Handler e,
                                 eAddHandler :: AddHandler e,
                                 eFilter :: e -> Bool}


type ParamMap a k e = M.Map k (a e)
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

--type HandlerMap k e = ParamMap Handler k e
type EventEntryMap k e = ParamMap EventEntry k e
type EventEntryMapBhv t k e = ModEvent t (EventEntryMap k e)

type EventMap k e = ParamMap AddHandler k e 
type EventMapBhv t k e = ModEvent t (EventMap k e)

type EventManager id k e = M.Map id (EventMap k e)
type EventEntryManager id k e = M.Map id (EventEntryMap  k e)


class (Ord k) => IDable e k where
        extractID :: e -> k

class Mergeable a e where allAddHandlers :: a -> [AddHandler e]
instance Mergeable (EventEntry e) e where allAddHandlers = pure . eAddHandler
instance Mergeable (AddHandler e) e where allAddHandlers = pure
instance Mergeable a e => Mergeable (M.Map k a) e
    where allAddHandlers = (allAddHandlers =<<) . M.elems

mergeEvents :: (Frameworks t, Mergeable a e) => Event t a -> Moment t (Event t e)
mergeEvents = mergeAddHandlers . (allAddHandlers <$>)

mergeAddHandlers :: Frameworks t => Event t [AddHandler e] -> Moment t (Event t e)
mergeAddHandlers eH = switchE <$> execute (makeAnyMom <$> eH)
    where makeAnyMom :: [AddHandler e] -> FrameworksMoment (AnyMoment Event e)
          makeAnyMom hL = FrameworksMoment $ (trimE . unions =<< mapM fromAddHandler hL)


newEventEntry :: (e -> Bool) -> IO (EventEntry e)
newEventEntry pred = do (eE,eF) <- newAddHandler
                        pure $ EventEntry eF eE pred
insertEntry :: (Ord k) => k -> a e -> ParamMap a k e -> ParamMap a k e
insertEntry  = M.insert

{-| Removes the entry from the event map |-}
deleteKey :: (Ord k) => k -> ParamMap a k e -> ParamMap a k e
deleteKey = M.delete

{-| Fires the value on the specified key event |-}
fireKey :: (IDable e k) => EventEntryMap k e -> e -> Maybe (IO ())
fireKey m e = fireKey' m (extractID e) e


fireKey' :: Ord k => EventEntryMap k e -> k -> e -> Maybe (IO ())
fireKey' m k e = join $ f <$> k `M.lookup` m
  where f entry = if eFilter entry e then Just (eFire entry e) else Nothing

{-| Extracts the event stream from a given key |-}
kEvents :: (Ord k) => EventEntryMap k e -> k -> Maybe (AddHandler e)
kEvents m k = eAddHandler <$> k `M.lookup` m

