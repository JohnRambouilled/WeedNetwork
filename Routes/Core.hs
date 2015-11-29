module Routes.Core where 

import Routes.Table

import Class
import PipePackets
import Neighbors
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Tree
import Data.Tree.Zipper
import Data.Maybe

data NeighBreak = NeighBreak {nbPartialRoad :: [UserID]}
data RoutingData = RoutingNode UserID
                 | RoutingLeaf PipeID
            deriving Eq

data RoutingTree = RoutingTree {leechsTree :: Tree RoutingData,
                                seedsTree :: Tree RoutingData}

--accumRoutingTable :: Event t (Request, EventC PipeMessage) -> Event t NeighBreak -> Moment t (BehaviorMod t (Tree RoutingData))

{- Sépare une route en deux routes : 
    - [Leachs] ++ [PipeID]
    - [Sources] ++ [PipeID]
-}
splitRoads :: UserID -> ([UserID], PipeID) -> ([RoutingData], [RoutingData])
splitRoads me (road,pID) = (leechRoad ++ [RoutingLeaf pID] , seedRoad ++ [RoutingLeaf pID]) 
  where (leechRoad, seedRoad ) = splitPartialRoads me road
splitPartialRoads :: UserID -> [UserID] -> ([RoutingData], [RoutingData])
splitPartialRoads me road = (leechRoad, seedRoad) 
  where (reversedLeechRoad, seedRoad') = span (/= me) road
        leechRoad = map RoutingNode (me:reverse reversedLeechRoad)
        seedRoad = map RoutingNode seedRoad' 


{- Insère les sous routes spécifiées -}
addRoad ::  ([RoutingData], [RoutingData]) -> RoutingTree -> RoutingTree --(Tree RoutingData,Tree RoutingData) -> (Tree RoutingData,Tree RoutingData)
addRoad (leechRoad, seedRoad) (RoutingTree leechT seedT) = RoutingTree (toTree $ insertRoad leechRoad leechT) (toTree $ insertRoad seedRoad seedT)


{- Retire les sous routes spécifiées -}
delRoad :: ([RoutingData], [RoutingData]) -> RoutingTree -> RoutingTree
delRoad (leechRoad,seedRoad) (RoutingTree leechT seedT) = RoutingTree (removeRoad leechRoad leechT) (removeRoad seedRoad seedT) 
  where removeRoad :: [RoutingData] -> Tree RoutingData -> Tree RoutingData
        removeRoad road tree = maybe tree id (toTree <$> removeSubRoad road tree)


buildRoutingTable ::  UserID -> Event (Request, EventC PipeMessage) -> Event NeighBreak -> MomentIO (BehaviorMod RoutingTree)
buildRoutingTable me reqE breakE = do routingTree <- newBehaviorMod (RoutingTree (makeTree [RoutingNode me]) (makeTree [RoutingNode me]))
                                      execute $ onRequest (bmModifier routingTree) <$> reqE
                                      reactimate $ onNeighBreak (bmModifier routingTree) <$> breakE
                                      pure routingTree

  where onRequest :: Modifier RoutingTree -> (Request, EventC PipeMessage) -> MomentIO ()                              
        onRequest mod (req, pipeC) = do let road = splitRoads me (reqRoad req, reqPipeID req) 
                                        liftIO $ mod $ addRoad road
                                        reactimate $ fmap (pure $ mod $ delRoad  road) (ceCloseEvent pipeC)
        onNeighBreak mod nb = mod $ delRoad (splitPartialRoads me $ nbPartialRoad nb)

    
