{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module UI.ShowClient where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad

import Class
import Routing
import Ressource
import Neighbors
import Pipes
import Client

 

type WidgetHandler = String -> String -> IO ()
showClient :: Frameworks t => Client t -> [WidgetHandler] -> Moment t ()
showClient c = zipWithM_ showModule (moduleList c)
  where showModule (n,e) h = reactimate $ h n <$> e

moduleList :: Client t -> [(String, Event t String)]
moduleList c = [("Pipes", showPipes c),
            ("Neighbors", showNeighborhood c),
            ("Answers", showRessourcesAns c),
            ("Relayed Research", showRessourcesRel c),
            ("Routing Local", showRoutingLocal c),
            ("Routing Relayed", showRoutingRelay c),
            ("Received Packets", showReceivedPackets c),
            ("Packets sent", showSentPackets c)]
            where showPipes :: Client t -> Event t String
                  showPipes = (showPipesManager <$>) . meChanges . pipesManager . clPipes
                        where showPipesManager = concatMap showSource . M.assocs
                              showSource (sID, e) = "\t Source : " ++ show sID ++ "\n\t\t" ++ concat (map ((++ "\n\t\t") . show) $ M.keys $ pmePipeMap e) ++ "\n\n"

                  showNeighborhood = showMap . nbhNeighMap . clNeighbors
                  showRessourcesAns = showMap . resAnswerMap . clRessources
                  showRessourcesRel = showMap . resRelayMap . clRessources
                  showRoutingLocal = showMap . routingLocMap . clRouting
                  showRoutingRelay = showMap . routingRelMap . clRouting
                  showReceivedPackets c = showPacket <$> clReceived c
                  showSentPackets c = showPacket <$> clToSend c

                  showPacket :: Packet -> String
                  showPacket (Left (Left a)) = show a
                  showPacket (Left (Right a)) = show a
                  showPacket (Right a) = show a

                  showMap mod = sm <$> meChanges mod
                        where sm = concat . map ((++ "\n") . show) . M.keys

moduleNameList = ["Pipes","Neighbors","Answers","Relayed Research","Routing Local","Routing Relayed","Received Packets","Packets sent"]
