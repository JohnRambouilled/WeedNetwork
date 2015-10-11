{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad



newtype Handle a = Handle {fire :: Handler a}
--newEvent' :: Reactive (Event e, Handler e)
--newEvent' = do (e, h) <- newEvent
  --             pure (e, Handler h)

data EventEntry t e = EventEntry { eFire :: Handler e,
                                   eEvent :: Event t e,
                                   eFilter :: e -> Bool}

type EventEntryManager t id k e = M.Map id (Behavior t (EventEntryMap t k e))



type ParamMap a k e = M.Map k (a e)
type Modifier a = (a -> a) -> IO ()

data ModEvent t a = ModEvent {meLastValue :: Behavior t a, 
                              meChanges :: Event t a,
                              meModifier :: Modifier a}
newModEvent :: Frameworks t => a -> Moment t (ModEvent t a)
newModEvent i = do (e, h) <- newEvent
                   let (e', b) = mapAccum i $ (\f a -> (f a, f a)) <$> e
                   pure $ ModEvent b e' h


type HandlerMap k e = ParamMap Handle k e
type EventEntryMap t k e = ParamMap (EventEntry t) k e
type EventEntryMapBhv t k e = ModEvent t (EventEntryMap t k e)
type EventMap t k e = ParamMap (Event t) k e --M.Map k (Event e)
type EventMapBhv t k e = ModEvent t (EventMap t k e)
type EventManager t id k e = M.Map id (EventMapBhv t k e)


class Mergeable t a e | a -> e where allEvents :: a -> Event t e

instance Frameworks t => Mergeable t (Event t e) e where allEvents = id
instance Mergeable t (Event t e, a) e where allEvents = fst
instance Mergeable t (EventEntry t e) e where allEvents = eEvent
--instance Mergeable t a e => Mergeable t (Behavior t a) (Future e) where allEvents bhv = switchE (allEvents <$> bhv)
instance Mergeable t a e => Mergeable t (M.Map k a) e where allEvents m = unions $ allEvents <$> M.elems m
--instance Mergeable t a e => Mergeable t (BhvTpl t a) e where allEvents = allEvents . fst

--type FutureEvent t a = Event t (Future a)

--class Frameworks t => Switchable t a e where switchEvents ::  a -> Moment t (Event t e)

--instance (Frameworks t, Mergeable t a e) => Switchable t (Event t a) e where 
  --              switchEvents e0 = let e1 = allEvents <$> e0
    --                                  e2 = trimE <$> e1
      --                                e3 = FrameworksMoment <$> e2
        --                          in do e4 <- execute e3
          --                              pure (switchE <$> e4)
                
                --switchE <$> execute (FrameworksMoment . trimE . allEvents <$> e0)
{-
switchEvents :: forall t t' e. Frameworks t => Event t (Moment t e) -> Moment t (Event t e)
switchEvents e0 = let e1 = e0
                      e2 = trimE <$> e1
                      e3 = FrameworksMoment <$> e2
                   in do e4 <- execute e3
                         pure (switchE <$> e4)
  -}

fm :: Frameworks t => Event t (forall t'.Frameworks t' => Moment t' e) -> Moment t (Event t e)
fm e = execute (FrameworksMoment <$> e)


--fm2 :: Frameworks t => Event t (AddHandler e) -> Moment t (Event t e)
--fm2 e = fm (fromAddHandler <$> e)

test :: AddHandler e -> (forall t'. Frameworks t' => Moment t' (AnyMoment Event e))
test h = do e <- fromAddHandler h
            trimE e




class (Ord k) => IDable e k where
        extractID :: e -> k


newEventEntry ::  Frameworks t => (e -> Bool) -> Moment t (EventEntry t e)
newEventEntry pred = do (eE,eF) <- newEvent
                        pure $ EventEntry eF eE pred
insertEntry :: (Ord k) => k -> a e -> ParamMap a k e -> ParamMap a k e
insertEntry  = M.insert

{-| Removes the entry from the event map |-}
deleteKey :: (Ord k) => k -> ParamMap a k e -> ParamMap a k e
deleteKey = M.delete

{-| Fires the value on the specified key event |-}
fireKey :: (IDable e k) => EventEntryMap t k e -> e -> Maybe (IO ())
fireKey m e = fireKey' m (extractID e) e


fireKey' :: Ord k => EventEntryMap t k e -> k -> e -> Maybe (IO ())
fireKey' m k e = join $ f <$> k `M.lookup` m
  where f entry = if eFilter entry e then Just (eFire entry e) else Nothing

{-| Extracts the event stream from a given key |-}
kEvents :: (Ord k) => EventEntryMap t k e -> k -> Maybe (Event t e)
kEvents m k = eEvent <$> k `M.lookup` m

{-| Merges the events from a given ID |-}
--idEvents :: (Ord id, Ord k) => EventEntryManager t id k e -> id -> Maybe (Event t e)
--idEvents mana id = switchE . fmap allEvents <$> idMapM  
--- where idMapM = id `M.lookup` mana


--swapB :: (Ord k) => Behaviour (Reactive (M.Map k a)) -> Reactive (Behaviour (M.Map k a))
--swapB mapB = hold M.empty $ execute $ value mapB


--swapBRM :: (Ord k) => Behaviour (M.Map k (Reactive e)) -> Reactive (Behaviour (M.Map k e))
--swapBRM mapB = swapB $ sequence <$> mapB
