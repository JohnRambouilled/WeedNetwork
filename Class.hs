{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Class where

import FRP.Sodium
import qualified Data.Map as M
import Control.Monad

type Handler a = a -> Reactive ()

data EventMapEntry e = EventMapEntry { eFire :: Handler e,
                                       eEvent :: Event e,
                                       eFilter :: e -> Bool}
data EventMap k e = EventMap {eventMap :: M.Map k (EventMapEntry e)}

type EventManager id k e = M.Map id (Behaviour (EventMap k e))


class (Ord k) => IDable e k | e -> k where
        extractID :: e -> k

insertEntry :: (Ord k) => k -> EventMapEntry e -> EventMap k e -> EventMap k e
insertEntry k e (EventMap m) = EventMap $ M.insert k e m

{-| Removes the entry from the event map |-}
deleteKey :: (Ord k) => k -> EventMap k e -> EventMap k e
deleteKey k (EventMap m) = EventMap (M.delete k m)

{-| Fires the value on the specified key event |-}
fireKey :: (IDable e k) => EventMap k e -> e -> Maybe (Reactive ())
fireKey m e = join $ f <$> extractID e `M.lookup` eventMap m
  where f entry = if eFilter entry e then Just (eFire entry e) else Nothing


{-| Extracts the event stream from a given key |-}
kEvents :: (Ord k) => EventMap k e -> k -> Maybe (Event e)
kEvents m k = eEvent <$> k `M.lookup` eventMap m

{-| Merges all the events of a given map |-}
allEvents' :: EventMap k e -> Event e
allEvents' m = foldr merge never $ eEvent <$> M.elems (eventMap m)

{-| Merges the events from a given ID |-}
idEvents :: (Ord id, Ord k) => EventManager id k e -> id -> Maybe (Event e)
idEvents mana id = switchE . fmap allEvents' <$> idMapM  
 where idMapM = id `M.lookup` mana

{-| Merges all the events of a given manager |-}
allEvents :: (Ord id, Ord k) => EventManager id k e -> Event e
allEvents mana = foldr merge never $ switchE . fmap allEvents' <$> M.elems mana



