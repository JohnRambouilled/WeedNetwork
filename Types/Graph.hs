{-# LANGUAGE TemplateHaskell #-}
module Types.Graph where

import Packets
import Types.Neighbours
import Types.Crypto

import qualified Data.Map as M
import Data.List
import Control.Monad.Cont
import Control.Monad
import Control.Lens
import Data.Maybe


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



data MCMark = MCMark {_mcIsVisited :: Bool,
                      _mcEnd :: [UserID]}
makeLenses ''MCMark

