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



genTestGraph :: [[Int]] -> IO TestGraph
genTestGraph neighList = do tchans <- atomically . forM [1..n] $ pure newTChan
                            cl <- forM (zip neighList [1..]) $ genClient tchans
                            forM_ (zip cl tchans) $ \(c,t) -> forkIO (react c t)
                            forM_ cl introduceThread
                            pure $ M.fromList (zip [1..] cl)
  where n = length neighList
        genClient tchans (neighs, i) = generateClient send
          where send :: RawData -> WeedMonad ()
                send d = forM_ neighs $ \i -> liftSTM (writeTChan (tchans !! i) d)
