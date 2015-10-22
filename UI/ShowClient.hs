{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module ShowClient where

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

compileClient :: [WidgetHandler] -> IO ()
compileClient wL = do actuate =<< compile cClient
                      
    where cClient :: Frameworks t => Moment t ()
          cClient = do (niE, niH) <- newEvent
                       (ndE, ndH) <- newEvent
                       (ppE, ppH) <- newEvent
                       c <- buildClient $ PacketEvents niE ndE ppE
                       showClient c wL

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
                        ("Routing Relayed", showRoutingRelay c)]
            where showPipes :: Client t -> Event t String
                  showPipes = (showPipesManager <$>) . meChanges . pipesManager . clPipes
                        where showPipesManager = concatMap showSource . M.assocs
                              showSource (sID, e) = "\t Source : " ++ show sID ++ "\n\t\t" ++ concat (map ((++ "\n\t\t") . show) $ M.keys $ pmePipeMap e) ++ "\n\n"


                  showNeighborhood :: Client t -> Event t String
                  showNeighborhood = showMap . nbhNeighMap . clNeighbors


                  showRessourcesAns :: Client t -> Event t String
                  showRessourcesAns = showMap . resAnswerMap . clRessources

                  showRessourcesRel :: Client t -> Event t String
                  showRessourcesRel = showMap . resRelayMap . clRessources


                  showRoutingLocal :: Client t -> Event t String
                  showRoutingLocal = showMap . routingLocMap . clRouting

                  showRoutingRelay :: Client t -> Event t String
                  showRoutingRelay = showMap . routingRelMap . clRouting


                  showMap :: Show k => ModEvent t (M.Map k a) -> Event t String
                  showMap mod = sm <$> meChanges mod
                        where sm = concat . map ((++ "\n") . show) . M.keys

