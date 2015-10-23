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
moduleList c = [("Pipes", showPipes),
            ("Neighbors", showNeighborhood),
            ("Answers", showRessourcesAns),
            ("Relayed Research", showRessourcesRel),
            ("Routing Local", showRoutingLocal),
            ("Routing Relayed", showRoutingRelay),
            ("Received Packets", showReceivedPackets 20),
            ("Packets sent", showSentPackets 20)]
            where 
                  showPipes = (showPipesManager <$>) . meChanges . pipesManager $ clPipes c
                        where showPipesManager = concatMap showSource . M.assocs
                              showSource (sID, e) = "\t Source : " ++ show sID ++ "\n\t\t" ++ concat (map ((++ "\n\t\t") . show) $ M.keys $ pmePipeMap e) ++ "\n\n"

                  showNeighborhood = showMap . nbhNeighMap $ clNeighbors c
                  showRessourcesAns = showMap . resAnswerMap $ clRessources c
                  showRessourcesRel = showMap . resRelayMap $ clRessources c 
                  showRoutingLocal = showMap . routingLocMap $ clRouting c
                  showRoutingRelay = showMap . routingRelMap $ clRouting c
                  showReceivedPackets n =  accumStrings n $ showPacket <$> clReceived c
                  showSentPackets n = accumStrings n $ showPacket <$> clToSend c

                  accumStrings :: Int -> Event t String -> Event t String
                  accumStrings n e = concat <$> (accumE [] $ f <$> e)
                        where f s l = take n $ (s ++ "\n\n") : l

                  showPacket :: Packet -> String
                  showPacket (Left (Left a)) = show a
                  showPacket (Left (Right a)) = show a
                  showPacket (Right a) = show a

                  showMap mod = sm <$> meChanges mod
                        where sm = concat . map ((++ "\n") . show) . M.keys

moduleNameList :: [String]
moduleNameList = ["Pipes","Neighbors","Answers","Relayed Research","Routing Local","Routing Relayed","Received Packets","Packets sent"]
