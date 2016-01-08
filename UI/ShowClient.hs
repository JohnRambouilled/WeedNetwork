{-# LANGUAGE ImpredicativeTypes, RankNTypes, FlexibleInstances #-}
module UI.ShowClient where

import Prelude hiding (showList)

import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Map as M
import Control.Monad
import qualified Data.Array as A
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Tree

import Routes.Core
import Class
import Routing
import Ressource
import Neighbors
import Pipes
import PipePackets
import Client
import Network
import UI.App

type Array = A.Array (Int,Int)
type ShowModule = (String, Array (BananWriter (AddHandler String)) )

launchApp :: ShowClient -> IO (MVar ())
launchApp showC = myForkIO $ buildApp showC
    where myForkIO :: IO () -> IO (MVar ())
          myForkIO io = do mvar <- newEmptyMVar
                           forkIO (io >> putMVar mvar ())
                           return mvar

renderClient :: [ShowModule] -> BananWriter ShowClient
renderClient = mapM renderModule
    where renderModule :: (String, Array (BananWriter (AddHandler String))) -> BananWriter (String, Array (AddHandler String))
          renderModule (name, arr) = do arr' <- sequence arr
                                        pure (name, arr')


showModuleList :: [ShowModule]
showModuleList = [("Routing", A.array ((1,1),(2,2)) [((1,1), showMap "ROUTING LOCAL" $ routingLocMap . clRouting),
                                                     ((1,2), showMap "ROUTING RELAY" $ routingRelMap . clRouting),
                                                     ((2,1), showMap "NEIGHBORS" $ nbhNeighMap . clNeighbors),
                                                     ((2,2), showPipes "PIPE MAP" $ bmBhvC . routingSourceMap . clRouting)]),
                  ("Network", A.listArray ((1,1),(2,1)) [showTree "LEECH TREE" $ fmap leechsTree . routingTree . clRouting,
                                                         showTree "SEEDS TREE" $ fmap seedsTree . routingTree . clRouting ]),
                  ("Ressources", A.array ((1,1),(2,1)) [((1,1), showMap "RESSOURCE LOCAL" $ bmBhvC . resLocalAnswerMap . clRessources),
                                                        ((2,1), showMap "RESSOURCE LISTEN" $ resListenMap . clRessources)]),
                  ("Logs", A.listArray ((1,1),(2,1)) [getClientEvent $ accumStrings "ROUTING LOG" 30 . routingLogs . clRouting,
                                                      getClientEvent $ accumStrings "RESSOURCE LOG" 30 . resLogs . clRessources]),
                  ("Packets", A.listArray ((1,1),(2,1)) [getClientEvent $ accumStrings "RECV" 30 . fmap showPacket . clReceived,
                                                         getClientEvent $ accumStrings "SEND" 30 . fmap showPacket . clToSend]) ]

    where showMap :: Show k => String -> (Client -> BehaviorC (M.Map k a)) -> BananWriter (AddHandler String)
          showMap name acc = extractAddHandler $ \c -> showList name . M.keys <$> bcChanges (acc c)
          
          showTree :: Show a => String -> (Client -> BehaviorC (Tree a)) -> BananWriter (AddHandler String)
          showTree name f = extractAddHandler $ \c -> drawTree . fmap show <$> bcChanges (f c)

          showPipes :: String -> (Client -> BehaviorC SourceMap) -> BananWriter (AddHandler String)
          showPipes name acc = getClientEvent $ \c -> showSourceMap name (bcChanges $ acc c)
          showSourceMap :: String -> Event SourceMap -> Handler String -> MomentIO ()
          showSourceMap name sm h = reactimate' . (fmap (h . showPipeMap) <$>) =<< changes . mapBhv =<< (fst <$> newBehavior M.empty)
            where mapBhv ::  Behavior (M.Map SourceID PipeMap) -> Behavior (M.Map SourceID PipeMap)
                  mapBhv b = switchB b $ sequenceA . fmap (bcLastValue . sePipeMap) <$> sm



showList :: Show k => String -> [k] -> String
showList name l = ((name ++"\n\n")++) . unlines $ show <$> l
showPipeMap :: (Show k, Show l) => M.Map k (M.Map l a) -> String
showPipeMap m = unlines ("PIPES\n\n":map f (M.toList m))
  where f (k,v) = show k ++ "\n"
                ++ unlines (map (\l -> "....." ++ show l) (M.keys v))

          

accumStrings :: String -> Int -> Event String -> Handler String -> MomentIO ()
accumStrings name n e h = do e'<- accumE [] $ f <$> e
                             reactimate $ h . ((name ++ "\n") ++) . concat <$> e'  
    where f s l = take n $ (s ++ "\n") : l
showPacket :: Packet -> String
showPacket (Left (Left a)) = show a
showPacket (Left (Right a)) = show a
showPacket (Right a) = show a


--showModuleList = [(Neighbors, A.Array ((1,1

{-

renderClient :: ClientInterface -> [A.Array (Int,Int) (AddHandler String)] --[A.Array (Int,Int) (AddHandler String)]
renderClient cInterface = [win1,win2,win3, win4]
    where [neigh,rLoc,rRel,pMap,resLocal, resListen, output, input, routLog, pipeLog] = showClientEvent' cInterface
          win1 = A.array ((1,1),(2,2)) [((1,1),rLoc),((1,2),rRel),
                                        ((2,1),neigh), ((2,2),pMap)]
          win2 = A.listArray ((1,1),(2,1)) [resLocal, resListen] --, output, routLog]
          win3 = A.listArray ((1,1),(2,1)) [pipeLog, routLog]
          win4 = A.listArray ((1,1),(2,1)) [output,input]
showClientEvent' :: ClientInterface -> [AddHandler String]
showClientEvent' cInterface = [dump "NEIGHBORS" cleNeighborsList, dump "ROUTING LOCAL" cleRoutingLocalList, dump "ROUTING RELAY" cleRoutingRelayedList]
                        ++ [showPipeMap <$> clePipeManager (ciEvents cInterface)] ++ [dump "RESSOURCE LOCAL" cleResLocalMap, dump "RESSOURCE LISTEN" cleResListenMap]
                        ++ [interpretAsHandler (accumStrings "SEND" 20) $ showPacket  <$> ciOutput cInterface, 
                           interpretAsHandler (accumStrings "RECV" 20) $ showPacket <$> cleReceivedMessage (ciEvents cInterface),
                           interpretAsHandler (accumStrings "ROUTING LOGS" 20) $ cleRoutingLogs $ ciEvents cInterface, 
                           interpretAsHandler (accumStrings "PIPES LOGS" 20) $ clePipeLogs $ ciEvents cInterface]
    where dump name f = showList name <$> f (ciEvents cInterface)

-}
