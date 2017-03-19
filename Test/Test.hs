module Test.Test where
import Client
import Types
import Packets
import Client.WeedMonad

import qualified Data.Map as M
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

type TestGraph = M.Map Int Client


dualTestGraph = genTestGraph [[1],[0]]

genTestGraph :: [[Int]] -> IO TestGraph
genTestGraph neighList = do tchans <- atomically . forM [0..n] $ pure newTChan
                            print "Creating Clients"
                            cl <- forM (zip neighList [0..]) $ genClient tchans
                            putStrLn $ "done : " ++ show (length cl)
                            print "Starting reaction" 
                            forM_ (zip cl tchans) $ \(c,t) -> forkIO (react c t)
                            print "Starting introduce threads"
                            forM_ cl $ forkIO . introduceThread
                            pure $ M.fromList (zip [0..] cl)
  where n = length neighList
        genClient tchans (neighs, i) = do c <- generateClient send
                                          pure c{clLogHandler = \l -> putStrLn ("Client " ++ show i ++ " ==> " ++ show l)}
          where send :: RawData -> WeedMonad ()
                send d = do logM "Test.Test" "send" Normal "Sending packet"
                            forM_ neighs sendTo
                    where sendTo i = liftSTM (writeTChan (tchans !! i) d)
