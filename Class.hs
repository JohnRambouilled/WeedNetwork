{-# LANGUAGE FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import FRP.Sodium
import qualified Data.Map as M
import Control.Monad

type Handler a = a -> Reactive ()

data EventEntry e = EventEntry { eFire :: Handler e,
                                       eEvent :: Event e,
                                       eFilter :: e -> Bool}
data EventEntryMap k e = EventEntryMap {eventMap :: M.Map k (EventEntry e)}

type EventEntryManager id k e = M.Map id (Behaviour (EventEntryMap k e))


type EventMap k e = M.Map k (Event e)
type EventMapBhv k e = Behavior (EventMap k e)
type EventManager id k e = M.Map id (EventMapBhv k e)
type EventManagerBhv id k e = Behavior (EventManager id k e)


class Mergeable a e | a -> e where allEvents :: a -> Event e
instance Mergeable (Event e) e where allEvents = id
instance Mergeable (EventEntry e) e where allEvents = eEvent
instance Mergeable (EventEntryMap k e) e where allEvents m = foldr merge never $ eEvent <$> M.elems (eventMap m)
instance Mergeable a e => Mergeable (Behavior a) e where allEvents bhv = switchE (allEvents <$> bhv)
instance Mergeable a e => Mergeable (M.Map k a) e where allEvents m = foldr merge never $ allEvents <$> M.elems m


class (Ord k) => IDable e k | e -> k where
        extractID :: e -> k


newEventEntry ::  (e -> Bool) -> Reactive (EventEntry e)
newEventEntry pred = do (eE,eF) <- newEvent
                        pure $ EventEntry eF eE pred
insertEntry :: (Ord k) => k -> EventEntry e -> EventEntryMap k e -> EventEntryMap k e
insertEntry k e (EventEntryMap m) = EventEntryMap $ M.insert k e m

{-| Removes the entry from the event map |-}
deleteKey :: (Ord k) => k -> EventEntryMap k e -> EventEntryMap k e
deleteKey k (EventEntryMap m) = EventEntryMap (M.delete k m)

{-| Fires the value on the specified key event |-}
fireKey :: (IDable e k) => EventEntryMap k e -> e -> Maybe (Reactive ())
fireKey m e = join $ f <$> extractID e `M.lookup` eventMap m
  where f entry = if eFilter entry e then Just (eFire entry e) else Nothing


{-| Extracts the event stream from a given key |-}
kEvents :: (Ord k) => EventEntryMap k e -> k -> Maybe (Event e)
kEvents m k = eEvent <$> k `M.lookup` eventMap m

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








