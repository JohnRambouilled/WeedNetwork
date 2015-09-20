{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import FRP.Sodium
import qualified Data.Map as M
import Control.Monad



newtype Handler a = Handler {fire :: a -> Reactive ()}
newEvent' :: Reactive (Event e, Handler e)
newEvent' = do (e, h) <- newEvent
               pure (e, Handler h)

data EventEntry e = EventEntry { eFire :: Handler e,
                                 eEvent :: Event e,
                                 eFilter :: e -> Bool}
--data EventEntryMap k e = EventEntryMap {eventMap :: M.Map k (EventEntry e)}

type EventEntryManager id k e = M.Map id (Behaviour (EventEntryMap k e))



type ParamMap a k e = M.Map k (a e)


type HandlerMap k e = ParamMap Handler k e
type EventEntryMap k e = ParamMap EventEntry k e
type EventMap k e = ParamMap Event k e --M.Map k (Event e)
type EventMapBhv k e = Behavior (EventMap k e)
type EventManager id k e = M.Map id (EventMapBhv k e)
type EventManagerBhv id k e = Behavior (EventManager id k e)



class Mergeable a e | a -> e where allEvents :: a -> Event e
instance Mergeable (Event e) e where allEvents = id
instance Mergeable (EventEntry e) e where allEvents = eEvent
--instance Mergeable (EventEntryMap k e) e where allEvents m = foldr merge never $ eEvent <$> M.elems (eventMap m)
instance Mergeable a e => Mergeable (Behavior a) e where allEvents bhv = switchE (allEvents <$> bhv)
instance Mergeable a e => Mergeable (M.Map k a) e where allEvents m = foldr merge never $ allEvents <$> M.elems m


class (Ord k) => IDable e k | e -> k where
        extractID :: e -> k


newEventEntry ::  (e -> Bool) -> Reactive (EventEntry e)
newEventEntry pred = do (eE,eF) <- newEvent
                        pure $ EventEntry (Handler eF) eE pred
insertEntry :: (Ord k) => k -> a e -> ParamMap a k e -> ParamMap a k e
insertEntry  = M.insert

{-| Removes the entry from the event map |-}
deleteKey :: (Ord k) => k -> ParamMap a k e -> ParamMap a k e
deleteKey = M.delete

{-| Fires the value on the specified key event |-}
fireKey :: (IDable e k) => EventEntryMap k e -> e -> Maybe (Reactive ())
fireKey m e = fireKey' m (extractID e) e


fireKey' :: Ord k => EventEntryMap k e -> k -> e -> Maybe (Reactive ())
fireKey' m k e = join $ f <$> k `M.lookup` m
  where f entry = if eFilter entry e then Just (fire (eFire entry) e) else Nothing

{-| Extracts the event stream from a given key |-}
kEvents :: (Ord k) => EventEntryMap k e -> k -> Maybe (Event e)
kEvents m k = eEvent <$> k `M.lookup` m

{-| Merges all the events of a given map |-}
--allEvents' :: EventEntryMap k e -> Event e
--allEvents' m = foldr merge never $ eEvent <$> M.elems (eventMap m)

{-| Merges the events from a given ID |-}
idEvents :: (Ord id, Ord k) => EventEntryManager id k e -> id -> Maybe (Event e)
idEvents mana id = switchE . fmap allEvents <$> idMapM  
 where idMapM = id `M.lookup` mana

{-| Merges all the events of a given manager |-}
--allEvents :: (Ord id, Ord k) => EventEntryManager id k e -> Event e
--allEvents mana = foldr merge never $ switchE . fmap allEvents' <$> M.elems mana

--allEventsBhv' :: (Ord k) => Behavior (EventEntryMap k e) -> Event e
--allEventsBhv' mBhv = switchE (allEvents' <$> mBhv)

--allEventsBhv :: (Ord id, Ord k) => Behavior (EventEntryManager id k e) -> Event e
--allEventsBhv mBhv = switchE (allEvents <$> mBhv)




--partitionMap :: (Ord i, Ord j) => M.Map (i, j) a -> M.Map i (M.Map j a)








