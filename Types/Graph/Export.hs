module Types.Graph.Export (exportGraph) where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Binary
import qualified Data.Map             as M
import           Data.Monoid
import           Types.Graph.RoadGraph
import           Types.Graph.Type
import           Types.Packet

{-| Une manière simple de sortir un fichier graviz pour représenter le graphe.|-}
exportGraph fileName g = writeFile fileName $ showGraph g

showVID (VertexID vID) = "X" ++ show vID

showVertex (vID,(vT,edges)) = unlines $ (\k -> "\t" ++ showVID vID++ " -> "++ showVID k ++ ";\n") <$> M.keys (_eMap edges)

showGraph g = "digraph G{\n"
            ++ unlines (showVertex <$> (M.toList $ _vMap g))
            ++ "}"



test = exportGraph "coucou.graph" $ deleteRoad id (VertexID <$> [0,3,4,5]) $
  pipe1 (pipe2 (pipe3 (pipe4 mempty)))
  where pipe1 = insertPipe Relayed (PipeID $ encode '1') $ me:(VertexID <$> [0,3,4,5])
        pipe2 = insertPipe Relayed (PipeID $ encode '2') $ me:(VertexID <$> [0,3,5,6])
        pipe3 = insertPipe Relayed (PipeID $ encode '3') $ (VertexID <$> [3,2,4,5,0,6,7]) ++ [me]
        pipe4 = insertPipe Relayed (PipeID $ encode '4') $ (VertexID <$> [2,5,8,0]) ++ [me]
