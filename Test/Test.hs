module Test.Test where
import UI.App
import Client
import Types
import Packets
import Client.WeedMonad
import Client.Ressource

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Binary
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

type TestGraph = [Client]


testRessource = RessourceID $ encode "Some Dank Ressource"

dualTestMain = do cl <- genTestGraph [[1],[0]]
                  --forkIO . buildApp $ dualShow cl
                  runWM (head cl) $ offerRessource testRessource $ encode "Dankest Dankness ever Danked"
                  threadDelay (10^6)
                  runWM (cl !! 1) $ researchSimpleRessource testRessource
                  threadDelay (10^6)


dualShow :: TestGraph -> ShowClient
dualShow = map showClient

showClient :: Client -> (String, A.Array (Int,Int) Displayer)
showClient c = (show $ clUserID c, A.listArray ((1,1), (1,2)) displayers)
  where displayers :: [Displayer]
        displayers = [dispMap clNeighbours, dispMap clRessources]
        dispMap :: Show k => (Client -> TVar (M.Map k a)) -> Displayer
        dispMap a = Displayer $ show . M.keys <$> readTVarIO (a c) 
                  
                  
genTestGraph :: [[Int]] -> IO TestGraph
genTestGraph neighList = do tchans <- atomically . forM [0..n] $ pure newTChan
                            print "Creating Clients"
                            cl <- forM (zip neighList [0..]) $ genClient tchans
                            putStrLn $ "done : " ++ show (length cl)
                            print "Starting reaction" 
                            forM_ (zip cl tchans) $ \(c,t) -> forkIO (react c t)
                            print "Starting introduce threads"
                            forM_ cl $ forkIO . introduceThread
                            pure cl
  where n = length neighList
        genClient tchans (neighs, i) = do c <- generateClient send
                                          pure c{clLogHandler = \l -> putStrLn ("Client " ++ show i ++ " ==> " ++ show l)}
          where send :: RawData -> WeedMonad ()
                send d = do logM "Test.Test" "send" Normal "Sending packet"
                            forM_ neighs sendTo
                    where sendTo i = liftSTM (writeTChan (tchans !! i) d)
