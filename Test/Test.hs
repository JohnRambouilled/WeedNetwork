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
import System.Console.ANSI

type TestGraph = [Client]


testRessource = RessourceID $ encode "Some Dank Ressource"
testColorMod = [[SetColor Foreground Vivid Green],
                [SetColor Foreground Vivid Red],
                [SetColor Foreground Vivid Blue]]
                


tripleTest = do c0:c1:c2:[] <- genTestGraph [[1],[0,2],[1]]
                runWM c0 $ do offerRessource rID $ encode "Dankest Dankness ever Danked"
                              addProtocol pID pEntry
                threadDelay (10^6)
                runWM c2 $ researchSimpleRessource rID
                threadDelay (10^6)
                senderM <- runWM c2 connect
                pure ()
                case senderM of
                    Nothing -> putStrLn "failed to connect"
                    Just (sID,sender) -> do putStrLn $ "connected!"
                                            cIDM <- runWM c2 $ openCommunication sID sender pID comEntry (encode "booya") 
                                            case cIDM of
                                              Nothing -> putStrLn "failed to create com"
                                              Just cID -> do putStrLn "communication established!"
                                                             runWM c2 $ sendComMessage sender cID (encode "Hello friend!")
                                                             threadDelay (15*10^6)
                                                             runWM c2 $ closeCommunication sID sender cID (encode "See you soon...")
                  
 
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
                                            cIDML <- forM [1..10] $ \_ -> do  cIDM <- runWM c1 $ openCommunication sID sender pID comEntry (encode "booya") 
                                                                              threadDelay (10^5)
                                                                              pure cIDM
                                            threadDelay (5*10^6)
                                            forM_ cIDML $ \cIDM -> case cIDM of Nothing -> pure ()
                                                                                Just cID -> runWM c1 $ closeCommunication sID sender cID (encode "See you soon...")
                                           {- case cIDM of
                                              Nothing -> putStrLn "failed to create com"
                                              Just cID -> do putStrLn "communication established!"
                                                             runWM c1 $ sendComMessage sender cID (encode "Hello friend!")
                                                             threadDelay (15*10^6)
                                                             runWM c1 $ closeCommunication sID sender cID (encode "See you soon...")
                                      -}
                  
                                   
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
genTestGraph neighList = do tchans <- forM [0..n] $ pure newTChanIO
                            logChan <- newTChanIO :: IO (TChan (IO ()))
                            forkIO $ printLogs logChan
                            print "Creating Clients"
                            cl <- forM (zip neighList [0..]) $ genClient logChan tchans
                            putStrLn $ "done : " ++ show (length cl)
                            forM_ cl $ print . clUserID
                            print "Starting reaction" 
                            forM_ (zip cl tchans) $ \(c,t) -> forkIO (react c t)
                            print "Starting introduce threads"
                            forM_ cl $ forkIO . introduceThread
                            pure cl
  where n = length neighList
        genClient lc tchans (neighs, i) = do c <- generateClient send
                                             pure c{clLogHandler = \l -> atomically . writeTChan lc $ do setSGR $ testColorMod !! i
                                                                                                         putStrLn ("Client " ++ show i ++ " : " ++ show (clUserID c) ++ " ==> " ++ show l)}
          where send :: RawData -> WeedMonad ()
                send d = do logM "Test.Test" "send" Normal "Sending packet"
                            forM_ neighs sendTo
                    where sendTo i = liftSTM (writeTChan (tchans !! i) d)
        printLogs :: TChan (IO ()) -> IO ()
        printLogs l = forever . join . atomically $ readTChan l
