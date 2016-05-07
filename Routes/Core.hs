{-# LANGUAGE DeriveGeneric #-}
module Routes.Core where 

import Routes.Table

import Class
import PipePackets
--import Neighbors
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Tree
import Data.Tree.Zipper hiding (isLeaf)
import Data.Maybe
import Data.Traversable
import GHC.Generics


data NeighBreak = NeighBreak {nbPartialRoad :: [UserID]} deriving (Generic, Show)

data RoutingData = RoutingNode UserID
                 | RoutingLeaf (PipeID,Channel PipePacket)
instance Show RoutingData where show (RoutingNode uID) = "User : " ++ show uID
                                show (RoutingLeaf (pID,_)) = "Pipe : " ++ show pID

instance Eq RoutingData where
         (RoutingNode uID) == (RoutingNode uID') = uID == uID'
         (RoutingLeaf (pID, _)) == (RoutingLeaf (pID',_)) = pID == pID'
         _ == _ = False

isLeaf (RoutingLeaf _) = True
isLeaf _ = False


fromLeaf (RoutingLeaf x) = x
fromLeaf _ = error "fromLeaf on node."

data RoutingTree = RoutingTree {leechsTree :: Tree RoutingData,
                                seedsTree :: Tree RoutingData}

--accumRoutingTable :: Event t (Request, Channel PipeMessage) -> Event t NeighBreak -> Moment t (BehaviorMod t (Tree RoutingData))

{- Sépare une route en deux routes : 
    - [Leachs] ++ [PipeID]
    - [Sources] ++ [PipeID]
-}
splitRoads :: UserID -> ([UserID], PipeID, Channel PipePacket) -> ([RoutingData], [RoutingData])
splitRoads me (road,pID, pipeC) = (leechRoad ++ [RoutingLeaf (pID, pipeC)] , seedRoad ++ [RoutingLeaf (pID, pipeC)]) 
  where (leechRoad, seedRoad ) = splitPartialRoads me road
splitPartialRoads :: UserID -> [UserID] -> ([RoutingData], [RoutingData])
splitPartialRoads me road = (leechRoad, seedRoad) 
  where (reversedLeechRoad, seedRoad') = span (/= me) road
        leechRoad = map RoutingNode (me:reverse reversedLeechRoad)
        seedRoad = map RoutingNode seedRoad' 


{- Insère les sous routes spécifiées -}
addRoad ::  ([RoutingData], [RoutingData]) -> RoutingTree -> RoutingTree --(Tree RoutingData,Tree RoutingData) -> (Tree RoutingData,Tree RoutingData)
addRoad (leechRoad, seedRoad) (RoutingTree leechT seedT) = RoutingTree (toTree $ insertRoad leechRoad leechT) (toTree $ insertRoad seedRoad seedT)

{- Retourne vrai si la route est dans la table -}
checkRoads me (RoutingTree leechT seedT) road = isJust (exploreTreePath leechRoad leechP) && isJust (exploreTreePath seedRoad seedP)
    where (leechRoad,seedRoad) = splitPartialRoads me road
          (leechP,seedP) = (fromTree leechT, fromTree seedT)


{- Retire les sous routes spécifiées -}
--delRoad :: ([RoutingData], [RoutingData]) -> RoutingTree -> Maybe (RoutingTree, Tree RoutingData, Tree RoutingData)
delRoad :: ([RoutingData], [RoutingData]) -> RoutingTree -> (RoutingTree, Maybe (Tree RoutingData), Maybe (Tree RoutingData))
delRoad (leechRoad,seedRoad) (RoutingTree leechT seedT) = let leechRet = removeSubRoad leechRoad leechT --RoutingTree (removeRoad leechRoad leechT) (removeRoad seedRoad seedT) 
                                                              seedRet = removeSubRoad seedRoad seedT

                                                          in (RoutingTree (maybe leechT fst leechRet) (maybe  seedT fst seedRet), snd <$> leechRet, snd <$> seedRet)




buildRoutingTable ::  UserID  -- Me
                  -> Handler (String)
                  -> Event (Request, Channel PipePacket)  -- Raw stream containing the requests relayed
                  -> Event NeighBreak -- Raw stream of breaks
                  -> MomentIO (BehaviorC RoutingTree, Event NeighBreak) -- the tree and the stream of breaks we have to send
buildRoutingTable me log reqE breakE = do routingTree <- newBehaviorMod (RoutingTree (makeTree [RoutingNode me]) (makeTree [RoutingNode me]))
                                          (relayE, relayF) <- newEvent
--                                          reactimate $ log . show . reqRoad . fst <$>  reqE
                                          execute  (onRequest (bmModifier routingTree) <$> reqE) >>= reactimate
                                          reactimate $ applyMod (onNeighBreak relayF) routingTree $ (me:) . nbPartialRoad <$> breakE
                                          pure (bmBhvC routingTree, relayE)

  where onRequest :: Modifier RoutingTree -> (Request, Channel PipePacket) -> MomentIO (IO ())                            
        onRequest mod (req, pipeC) = do let road = splitRoads me (reqRoad req, reqPipeID req, pipeC) 
                                        reactimate $ fmap (pure $ mod $ fst3.delRoad  road) (chanCloseE pipeC)
                                        pure (mod $ addRoad road)
        onNeighBreak :: Handler NeighBreak -> Modifier RoutingTree -> RoutingTree -> [UserID] -> IO ()
        onNeighBreak nbFire mod tr partialRoad = case delRoad (splitPartialRoads me partialRoad) tr of
                                                      (routingTree', Just rmLeech, _) -> do closeTree mod routingTree' rmLeech >> nbFire (NeighBreak partialRoad)
                                                      (routingTree',_, Just rmSeed) -> closeTree mod routingTree' rmSeed >> nbFire (NeighBreak partialRoad)
                                                      (routingTree',_,_) -> mod $ pure routingTree'
                                                      
        closes t = filter isLeaf $ flatten t
        closeTree :: Modifier RoutingTree -> RoutingTree -> Tree RoutingData -> IO ()
        closeTree mod newTr closeTr  = do mod $ pure newTr
                                          sequence_ $ chanCloseH . snd . fromLeaf <$> closes closeTr
                                          pure ()

    

fst3 (x,_,_) = x
