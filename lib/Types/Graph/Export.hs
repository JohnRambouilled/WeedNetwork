module Types.Graph.Export (exportGraph,showVID) where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Binary
import           Data.Binary
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Monoid
import           Types.Graph.RoadGraph
import           Types.Graph.Type
import           Types.Packets

{-| Une manière simple de sortir un fichier graviz pour représenter le graphe.|-}
exportGraph fileName g = writeFile fileName $ showGraph g

showVID (VertexID vID) = "X" ++ show ((decode vID) :: Int)

getVertex :: (VertexID, (VertexT, Edges a)) -> [(String, String, Bool)]
getVertex (vID,(vT,edges)) = convertEdge <$> M.keys (_eMap edges)
  --unlines $ (\k -> "\t" ++ showVID vID++ " -> "++ showVID k ++ ";\n") <$> M.keys (_eMap edges)
  where pipes = M.elems $ _vPipes $ _pipesT vT
        prevs = catMaybes $ _previous <$> pipes
        nexts = catMaybes $ _next <$> pipes
        convertEdge k
          | k  `elem` (prevs ++ nexts) = (showVID vID, showVID k,True)
          | otherwise = (showVID vID,showVID k,False)

getAllEdges g = sortTuple <$> (concat $ getVertex <$> (M.toList $ _vMap g))
  where sortTuple a@(vID,vID',b)
          | vID < vID' = a
          | otherwise = (vID',vID,b)

filterAllEdges g = [ei | ei@(vID,vID',b) <- edges, b || null [wID' | (wID,wID',e) <- edges, vID == wID, vID' == wID', e]]
  where edges = nub $ getAllEdges g

showEdge (vID,vID',True) = "\t" ++ vID ++ " -> " ++ vID' ++ " [dir=none color=\"green\"];\n"
showEdge (vID,vID',False) = "\t" ++ vID ++ " -> " ++ vID' ++ " [dir=none];\n"



{-
showVertex (vID,(vT,edges)) = unlines $ drawEdge <$> M.keys (_eMap edges)
  --unlines $ (\k -> "\t" ++ showVID vID++ " -> "++ showVID k ++ ";\n") <$> M.keys (_eMap edges)
  where pipes = M.elems $ _vPipes $ _pipesT vT
        prevs = catMaybes $ _previous <$> pipes
        nexts = catMaybes $ _next <$> pipes
        drawEdge k
          | k  `elem` (prevs ++ nexts) = "\t" ++ showVID vID ++  " -> " ++ showVID k ++ " [dir=none color=\"green\"];\n"
          | otherwise = "\t" ++ showVID vID ++ " -> " ++ showVID k ++ " [dir=none];\n"
-}


showGraph g = "digraph G{\n"
            ++ unlines (showEdge <$> filterAllEdges g)
            ++ "}"

type E = (VertexID, VertexID, Bool)


{-
test me = exportGraph "coucou.graph" $ deleteRoad id (VertexID <$> [0,3,4,5]) $
  pipe1 (pipe2 (pipe3 (pipe4 mempty)))
  where pipe1 = insertPipe Relayed (PipeID $ encode '1') $ me:(VertexID <$> [0,3,4,5])
        pipe2 = insertPipe Relayed (PipeID $ encode '2') $ me:(VertexID <$> [0,3,5,6])
        pipe3 = insertPipe Relayed (PipeID $ encode '3') $ (VertexID <$> [3,2,4,5,0,6,7]) ++ [me]
        pipe4 = insertPipe Relayed (PipeID $ encode '4') $ (VertexID <$> [2,5,8,0]) ++ [me]
-}
