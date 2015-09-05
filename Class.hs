{-# LANGUAGE MultiParamTypeClasses #-}
module Class where

import FRP.Sodium
import qualified Data.Map as M

type Handler a = a -> Reactive ()

data EventMapEntry e = EventMapEntry { eFire :: Handler e,
                                       eEvent :: Event e}
data EventMap k e = EventMap {eventMap :: M.Map k (EventMapEntry e)}

type EventManager id k e = M.Map id (Behaviour (EventMap k e))

{-| Removes the entry from the event map |-}
deleteKey :: (Ord k) => k -> EventMap k e -> EventMap k e
deleteKey k (EventMap m) = EventMap (M.delete k m)

{-| Fires the value on the specified key event |-}
fireKey :: (Ord k) => EventMap k e -> k -> e -> Maybe (Reactive ())
fireKey m k e = ($e) . eFire <$> k `M.lookup` eventMap m


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





