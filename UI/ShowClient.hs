{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module UI.ShowClient where

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import qualified Data.Array as A

import Class
import Routing
import Ressource
import Neighbors
import Pipes
import Client
import Client
import Network
import UI.App




renderClient :: ClientEvents -> IO ()--[A.Array (Int,Int) (AddHandler String)]
renderClient cEvents = buildApp [("LAYER 2",win1),("LAYER3",win2)]
    where [neigh,rLoc,rRel,pMap] = showClientEvent' cEvents
          win1 = A.array ((1,1),(4,4)) $ [((1,1),rLoc),((1,2),rRel),
                                            ((2,1),neigh)]
          win2 = A.array ((1,1),(1,1)) $ [((1,1),pMap)]
showClientEvent' :: ClientEvents -> [AddHandler String]
showClientEvent' cEvents = [dump "NEIGHBORS" cleNeighborsMap, dump "ROUTING LOCAL" cleRoutingLocalMap, dump "ROUTING RELAY" cleRoutingRelayedMap]
                        ++ [showPipeMap <$> clePipeManager cEvents]
    where dump name f = showMapKeys name <$> f cEvents


showMapKeys :: Show k => String -> M.Map k a -> String
showMapKeys name m = name ++"\n\n" ++ (unlines $ show <$> M.keys m)
showPipeMap :: (Show k, Show l) => M.Map k (M.Map l a) -> String
showPipeMap m = unlines $ ("PIPES\n\n":map f (M.toList m))
  where f (k,v) = show k ++ "\n"
                ++ unlines (map (\l -> "\t" ++ show l) (M.keys v))

          
