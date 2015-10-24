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
moduleList c = makeList [showPipes, showNeighborhood,
                         showRessourcesAns, showRessourcesRel,
                         showRoutingLocal, showRoutingRelay,
                         showReceivedPackets 20, showSentPackets 20,
                         showLogs 25]
            where makeList = zip  moduleNameList .  moduleShowList 
                  showPipes = (showPipesManager <$>) . meChanges . pipesManager $ clPipes c
                        where showPipesManager pm = concat $ showSource <$> M.assocs pm
                              showSource (sID, e) = " Source : " ++ show sID ++ "\n" ++ (showKeys $ pmePipeMap e)

                  showNeighborhood = showMap . nbhNeighMap $ clNeighbors c
                  showRessourcesAns = showMap . resAnswerMap $ clRessources c
                  showRessourcesRel = showMap . resRelayMap $ clRessources c 
                  showRoutingLocal = showMap . routingLocMap $ clRouting c
                  showRoutingRelay = showMap . routingRelMap $ clRouting c
                  showReceivedPackets n =  accumStrings n $ showPacket <$> clReceived c
                  showSentPackets n = accumStrings n $ showPacket <$> clToSend c
                  showLogs n = accumStrings n $ unions [routingLogs $ clRouting c,
                                                        pipesLogs $ clPipes c]

                  accumStrings :: Int -> Event t String -> Event t String
                  accumStrings n e = concat <$> (accumE [] $ f <$> e)
                        where f s l = take n $ (s ++ "\n\n") : l

                  showPacket :: Packet -> String
                  showPacket (Left (Left a)) = show a
                  showPacket (Left (Right a)) = show a
                  showPacket (Right a) = show a


                  showMap mod = showKeys <$> meChanges mod
                  showKeys :: Show k => M.Map k a -> String
                  showKeys = concat . map ((++ "\n") . show) . M.keys

moduleShowCount = length $ moduleShowList [1..] :: Int
moduleShowList :: [a] -> [a]
moduleShowList = map snd . filter fst . zip [True, True, True, False, True, False, True, True, True]
moduleNameList :: [String]
moduleNameList = moduleShowList ["Pipes","Neighbors","Answers","Relayed Research","Routing Local","Routing Relayed","Received Packets","Packets sent","Logs"]
