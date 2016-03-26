{-# LANGUAGE DeriveGeneric,MultiParamTypeClasses #-}
module AccessManager where


import Class
import Crypto
import Routing
import PipePackets
import Timer
import Pipes
import Ressource

import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana
import Reactive.Banana.Frameworks

import GHC.Generics

type RessourceEntry = [SourceID]
type RoadEntry = (DHPubKey, [Road])


type RoadMap = M.Map RoadID (Road, CloseEvent)

data AccessManager = AccessManager {amRessourceMap :: BehaviorC (M.Map RessourceID RessourceEntry),
                                    amRoadMap :: BehaviorC (M.Map SourceID RoadEntry),
                                    amPipesMap :: SourceMap}

{-
data RessourcePolicy = MonoPipe Int | MultiPipes Int Int

type RessourceEntry = BehaviorC (M.Map SourceID CloseEvent)

type RessourceMapBhv = BehaviorC (M.Map RessourceID RessourceEntry)

data AccessManager = AccessManager {amRessourceMap :: RessourceMapBhv,
                                    amNewRoadEvent :: Event NewRoad}
                                    

buildRessourceEntry :: UserID -> Routing -> Handler NewRoad -> (RessourceID, Event Answer) -> MomentIO (RessourceEntry)
buildRessourceEntry me rout nrH (rID,ansE) = do sourceL <- newBehaviorMod []
                                                reactimate =<< execute (apply (onAnswerB sourceL) ansE)
                                                bmBhvC sourceL
    where onAnswerB :: BehaviorMod (M.Map SourceID CloseEvent) -> Behavior (Answer -> IO ())
          onAnswerB bm = onAnswer <$> bmLastValue bm <*> bcLastValue (routingTree rout) <*> switchSourceMap

          onAnswer :: (M.Map SourceID CloseEvent) -> RoutingTree -> M.Map SourceID (Handler NewPipe, PipeMap) -> Answer -> MomentIO (IO ())
          onAnswer modL modS sourceL routTree sourceMap ans = let nSources = length (M.keys sourceL) in
            case rID `M.lookup` sourceL of
                Nothing -> case rID `M.lookup` sourceMap of
                            Nothing -> when (chooseAnswer ans nSource M.empty) $ insertNewSource modS (answerToNewRoad me ans)
                            Just (h,pm) -> when (chooseAnswer ans nSource pm) $ pure (h $ answerToNewRoad me ans)
                Just ce -> 
            

          switchSourceMap :: Behavior (M.Map SourceID (Handler NewPipe, PipeMap))
          switchSourceMap = switchB $ sequenceA . fmap buildEntry <$> bmLastValue sourceMap
            where buildEntry :: SourceEntry -> Behavior (Handler NewPipe, PipeMap)
                  buildEntry se = (,) (seAddPipe se) <*> sePipeMap se
          sourceMap = routingSourceMap rout






-}



answerToNewRoad :: UserID -> Answer -> NewRoad
answerToNewRoad uID = NewRoad <$> (uID :) . ansRoad <*> cResSourceDHKey . ansCert <*> ansSourceID <*> pure (encode "wooobdidoo")



