module Test.Test where
import Client
import Types
import Packets
import Client.WeedMonad
import Client.Ressource

import qualified Data.Map as M
import Data.Binary
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

type TestGraph = [Client]


testRessource = RessourceID $ encode "Some Dank Ressource"

dualTestMain = do cl <- genTestGraph [[1],[0]]
                  runWM (head cl) $ offerRessource testRessource $ encode "Dankest Dankness ever Danked"
                  threadDelay (10^6)
                  runWM (cl !! 1) $ sendSimpleResearch testRessource
                  
                  
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
