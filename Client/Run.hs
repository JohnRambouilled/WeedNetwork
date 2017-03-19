module Client.Run where


import Types
import Packets
import Client.Crypto
import Client.Neighbours
import Client.Pipes
import Client.WeedMonad

import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import System.Random
import Data.Time.Clock.POSIX
import qualified Data.Map as M

generateClient :: Sender -> IO Client
generateClient send = do keys <- generateKeyPair
                         let uID = computeHashFromKey $ fst keys
                         rndGen <- getStdGen
                         t <- getPOSIXTime
                         Client uID keys send <$> newTVarIO t
                                              <*> newTVarIO rndGen
                                              <*> newTVarIO mempty
                                              <*> newTVarIO M.empty
                                              <*> newTVarIO M.empty
                                              <*> newTVarIO M.empty
                                              <*> newTVarIO M.empty
                                              <*> newTVarIO M.empty
                                              <*> newTVarIO M.empty
                                              

onLayer1 :: L1 -> WeedMonad ()
onLayer1 (L1Intro intro) = onNeighIntro intro
onLayer1 (L1Data  ndata) = onNeighData  ndata
onLayer1 (L1Pipe   pipe) = onPipePacket pipe


react :: Client -> TChan L1 -> IO ()
react c inChan = forever $ runWM c action
  where action = liftSTM (readTChan inChan) >>= onLayer1
  
