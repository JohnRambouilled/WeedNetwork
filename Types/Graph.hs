{-# LANGUAGE TemplateHaskell #-}

module Types.Graph where

import Types.Neighbour
import qualified Data.Map as M
import Data.List
import Control.Monad.Cont
import Control.Monad
import Control.Lens
import Data.Maybe

data RessourceID = RessourceID
    deriving Eq
data DHPubKey = DHPubKey

data SourceEntry = SourceEntry {_seRessourceIDs :: [RessourceID],
                                _seDHKey :: DHPubKey}

makeLenses ''SourceEntry
--data GraphEntry = GraphEntry {geNeighs :: [UserID],
--                             geSourceEntry :: Maybe SourceEntry}

class (Eq a) => Node a where nodeNeigh :: Lens a a [UserID] [UserID]
                             nodeSourceM :: Lens a a (Maybe SourceEntry) (Maybe SourceEntry)



instance (Node node, Eq mark) => Node (node,mark) where --nodeNeigh  = lens (\(node, _) -> node ^. nodeNeigh) (\(node,mark) neighs -> (set nodeNeigh neighs node,mark))
                                               nodeNeigh = _1 . nodeNeigh 
                                               nodeSourceM = _1 . nodeSourceM


data Graph a = Graph {runGraph :: M.Map UserID a}                              

type MarkedGraph node mark = M.Map UserID (node, mark)

markB :: Node a => (a, Bool) -> (a,Bool)
markB = set _2 True


killNode :: Node a => Graph a -> UserID -> Graph a
killNode graph uID  = case nodeM of Nothing -> error "blabla"
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



data MCMark = MCMark {_mcIsVisited :: Bool,
                      _mcEnd :: [UserID]}
makeLenses ''MCMark

mcMarkVisited :: (Node a) => UserID -> MarkedGraph a MCMark -> MarkedGraph a MCMark
mcMarkVisited uID gr = M.adjust (set (_2 . mcIsVisited) True) uID gr

monteCarlo :: (Node a) =>  (MarkedGraph a MCMark  -> a -> (UserID,MCMark, a)) -> (Int,Int) -> MarkedGraph a MCMark -> [UserID] -> (UserID,MCMark,a) -> Maybe [UserID] 
monteCarlo curNext (min,max) graph road (curID,curMark,curNode) 
        | min > max = Nothing
        | not $ null (_mcEnd curMark) = pure $ (_mcEnd curMark ++ road)
        | _mcIsVisited curMark = error "montecarlo visite un noeud marqué"
        | otherwise = let nextStuff = curNext graph curNode 
                      in  monteCarlo curNext (min+1,max) (mcMarkVisited curID graph)(curID:road) nextStuff 






{-
--Does it works?? 
--Extrait le sous-graphe menant à une liste de sources donnée
extractGraph :: GraphModule -> [UserID] -> Cont r GraphModule
extractGraph graph sources = GraphModule . snd <$> foldM markNode (markedGraph, M.empty) sources
    where markedGraph = (\e -> (False,e)) <$> runGraph graph
          --markNode :: (MarkedGraph, MarkedGraph) -> UserID -> Cont r (MarkedGraph, MarkedGraph)
          markNode (g,res) sID = case sID `M.lookup` g of
                                    Nothing -> error "[critical] : graph : unknow node"
                                    Just (True, _) -> pure (g,res)
                                    Just (False, e) -> let g' = M.insert sID (True,e) g
                                                           res' = M.insert sID e res
                                                       in do foldM markNode (g', res') $ gePrevious e
                                                      

mergeGraph :: UserID -> UserID -> [UserID] -> GraphModule -> GraphModule -> GraphModule
mergeGraph me you sources myGraph yourGraph = myGraph'
    where 

-- Elimine un noeud, et ses voisins recursivement
killNode :: GraphModule -> UserID -> Cont r GraphModule
killNode graph uID = case nodeM of
                        Nothing -> pure graph
                        Just node -> do graph2 <- foldM (\a e ->  killFromNexts a uID e) (GraphModule graph1) (gePrevious node) 
                                        graph3 <- foldM  (\a e ->  killFromPrevious a uID e) graph2 (geNexts node) 
                                        pure graph3
                                       
    where (nodeM,graph1) = M.updateLookupWithKey (pure . pure $ Nothing) uID $ runGraph graph
          
-- Élimine un userID de la liste des nexts d'une entrée (et se supprime s'il n'a plus de nexts)          
killFromNexts :: GraphModule -> UserID -> UserID -> Cont r GraphModule
killFromNexts graph killedID uID = case nodeM of
                                        Nothing -> error "[critical] : graph : unknown next"
                                        Just node -> if null $ geNexts node
                                                        then killNode (GraphModule graph1) uID
                                                        else pure (GraphModule graph1)
    where (nodeM,graph1) = M.updateLookupWithKey adjustF uID $ runGraph graph
          adjustF _ node = Just $ node{geNexts = delete killedID (geNexts node)}

-- Idem que precedemment, mais pour les prévious
killFromPrevious :: GraphModule -> UserID -> UserID -> Cont r GraphModule
killFromPrevious graph killedID uID = case nodeM of
                                        Nothing -> error "[critical] : graph : unknown previous"
                                        Just node -> if null $ gePrevious node
                                                        then killNode (GraphModule graph1) uID
                                                        else pure (GraphModule graph1)
    where (nodeM,graph1) = M.updateLookupWithKey adjustF uID $ runGraph graph
          adjustF _ node = Just $ node{gePrevious = delete killedID (gePrevious node)}



-}


