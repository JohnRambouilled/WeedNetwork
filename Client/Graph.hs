module Client.Graph where

import Types
import Packets

import qualified Data.Map as M
import Data.List
import Control.Monad.Cont
import Control.Monad
import Control.Lens
import Data.Maybe



monteCarlo :: (Node a) =>  (MarkedGraph a MCMark  -> a -> (UserID,MCMark, a)) -> (Int,Int) -> MarkedGraph a MCMark -> [UserID] -> (UserID,MCMark,a) -> Maybe [UserID] 
monteCarlo curNext (min,max) graph road (curID,curMark,curNode) 
        | min > max = Nothing
        | not $ null (_mcEnd curMark) = pure (_mcEnd curMark ++ road)
        | _mcIsVisited curMark = error "montecarlo visite un noeud marqué"
        | otherwise = let nextStuff = curNext graph curNode 
                      in  monteCarlo curNext (min+1,max) (mcMarkVisited curID graph)(curID:road) nextStuff 


mcMarkVisited :: (Node a) => UserID -> MarkedGraph a MCMark -> MarkedGraph a MCMark
mcMarkVisited uID gr = M.adjust (set (_2 . mcIsVisited) True) uID gr

markB :: Node a => (a, Bool) -> (a,Bool)
markB = set _2 True


killNode :: Node a => Graph a -> UserID -> Graph a
killNode graph uID  = case nodeM of Nothing -> error ("Node not found : " ++ show uID)
                                    Just node -> foldr (removeNeigh uID) (Graph graph1) (view nodeNeigh node) 
    where (nodeM,graph1) = M.updateLookupWithKey adjustF uID $ runGraph graph
          adjustF _ _ = Nothing



removeNeigh :: (Node a) => UserID -> UserID -> Graph a -> Graph a
removeNeigh killedID uID graph = case nodeM of Nothing -> error "Invalid graph"
                                               Just node -> if keepP node then Graph graph1
                                                                         else killNode (Graph graph1) uID
    where (nodeM,graph1) = M.updateLookupWithKey adjustF uID $ runGraph graph
          adjustF _ node = Just $ over nodeNeigh (delete killedID) node

          keepP node = case view nodeNeigh node of
                        [] -> False
                        [x] -> isJust $ view nodeSourceM node
                        otherwise -> True 

mergeGraph :: (Node a) => Graph a -> Graph a -> Graph a
mergeGraph bigger smaller = Graph $ M.unionWith f (runGraph bigger) (runGraph smaller)
    where f node node' = let neigh' = view nodeNeigh node'
                             sourceM' = view nodeSourceM node'
                             unionSource :: Maybe SourceEntry -> Maybe SourceEntry
                             unionSource sourceM = case catMaybes [sourceM,sourceM'] of
                                                        [] -> Nothing
                                                        [x] -> Just x
                                                        [mine,yours] -> Just $ mergeSourceEntry mine yours
                         in  over nodeSourceM unionSource $ over nodeNeigh (union neigh') node
          mergeSourceEntry mine yours = SourceEntry (_seRessourceIDs mine `union` _seRessourceIDs yours) (_seDHKey mine)




