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

dualTestMain = do c0:c1:_ <- genTestGraph [[1],[0]]
                  --forkIO . buildApp $ dualShow cl
                  runWM c0 $ do offerRessource rID $ encode "Dankest Dankness ever Danked"
                                addProtocol pID pEntry
                  threadDelay (10^6)
                  runWM c1 $ researchSimpleRessource rID
                  threadDelay (10^6)
                  senderM <- runWM c1 connect
                  case senderM of
                    Nothing -> putStrLn "failed to connect"
                    Just (sID,sender) -> do putStrLn $ "connected!"
                                            cIDM <- runWM c1 $ openCommunication sID sender pID comEntry (encode "booya") 
                                            case cIDM of
                                              Nothing -> putStrLn "failed to create com"
                                              Just cID -> do putStrLn "communication established!"
                                                             runWM c1 $ sendComMessage sender cID (encode "Hello friend!")
                                      
                  
                                   
   where rID = testRessource
         cID = ComID 42
         pID = ProtocolID 42
         pEntry sID d = pure comEntry
         comEntry = ComEntry callback
           where callback pID cm = print pID
         connect :: WeedMonad (Maybe (SourceID,PipeSender))
         connect = do sIDs <- lookupSources rID 
                      case sIDs of
                        [] -> pure Nothing
                        sID:_ -> do r <- lookupRoad sID
                                    kM <- getSourceKey rID sID
                                    case kM of
                                      Nothing -> pure Nothing
                                      Just k -> openPipe r (encode "Give me some of this Dank Shit") k >>= (\pM -> case pM of Nothing -> pure Nothing
                                                                                                                              Just p -> fmap ((,) $ sID) <$>  genPipeSender p)
                                                                     


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
                                          pure c{clLogHandler = \l -> putStrLn ("Client " ++ show (clUserID c) ++ " ==> " ++ show l)}
          where send :: RawData -> WeedMonad ()
                send d = do logM "Test.Test" "send" Normal "Sending packet"
                            forM_ neighs sendTo
                    where sendTo i = liftSTM (writeTChan (tchans !! i) d)
