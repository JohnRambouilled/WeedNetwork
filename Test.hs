{-# LANGUAGE FlexibleContexts #-}
module Test where

import Control.Monad.State
import Control.Monad.Writer
import Control.Concurrent
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.List
import Data.Binary
import System.Random (randomIO)
import Control.Monad.RWS.Lazy

import Client.Class
import Client.Crypto
import Client.Packet
import Client.Routing
import Client.Sources
import Client.Ressource
import Client.Neighborhood
import Client.Pipes
import Proxy.RoadChoice
import Proxy.TCP
import Proxy.Socks
import Proxy.UDP
import Gateway hiding (keepL)
import Client.Protocol
import Timer
import Client
import Client.Communication
import Log
import Network.Socket
import Crypto.Random



data TestClient = TestClient {client :: Client,
                              input :: TChan RawData,
                              run :: IO ()} 




newTestClient ::  MVar RandomGen -> SendFunction -> IOLog Client
newTestClient gV send = do (pubkey,privkey) <- genKeyPairMVar gV
                           keepLog TestLog Normal "Creating client"
                           let me = SourceID $ KeyHash $ pubKeyToHash pubkey
                           liftIO $ genClient gV me privkey pubkey send



type ClientBehaviour = Client -> SendFunction -> IO ()



testFullGraph :: [ClientBehaviour] -> [LogFunction] -> IO [TestClient]
testFullGraph cL lL = newTest $ zipWith3 genNeigh [0..] cL lL
    where genNeigh i bH l = (filter (/=i) [0.. (length cL - 1)], bH, l)

{-
fullGraphMain :: Int -> IO ()
fullGraphMain n = do testCL <- testFullGraph =<< mapM bhvL [1..n]
                     let dumpClients = concat <$> (forM testCL $ (fst <$>) . runStateT dumpClient . client)
                     repeatEach (ctimer . client $ head testCL) (putStrLn =<< dumpClients) 2 >> pure ()
                     mVL <- forM testCL $ runChildren . run
                     dumpClients
                     forM_ mVL readMVar
    where bhvL i = do lchRID <- randomIO :: IO Int
                      pure $ testBhvr [RessourceID . encode $ i] [RessourceID . encode $ 1 + mod lchRID n]

testBhvr :: [RessourceID] -> [RessourceID] -> ClientBehaviour
testBhvr sdR lchR c send = do introduceThread c send "Seed Hello"
                              forM sdR $ flip (offerRessourceID c) $ encode "C'est de la bonne"
                              insertProtoCallback c inetUDPProtoID $ ProtoCallback testProtoCallback
                              forM_ lchR $ \rID -> do sV <- atomically $ newTVar []
                                                      connectToRessource c sV (proxyRoadChoice (csources c) sV) rID
                              forM_ lchR $ sendRes c send
    where testProtoCallback d (wF, bF) = do keepLog TestLog Important "Protocallback called"
                                            pure $ Just (Callback $ \d -> void (runWriteFun wF $ d), BrkClbck $ \_ -> pure ()) 
          
-}



--testChaine :: [ClientBehaviour] -> IO [TestClient]
--testChaine cL = newTest $ zipWith genNeigh [0..] cL
 --   where genNeigh i bh = if i == 0 then ([1], bh)
   --                                 else if i + 1 == length cL then ([i - 1], bh)
     --                                                          else ([i - 1, i + 1], bh)



tcp_socketName = "testounet_tcp.socket"
leechMain :: LogFunction -> ClientBehaviour  
leechMain logf c send = do  introduceThread c send "Leech Hello"
                            logf $ [LogMsg Normal TestLog "enregistrement de la répétition de recherche"]
                            logf $ [LogMsg Normal TestLog "démarrage du proxy "]
                            repeatEach (ctimer c) (void . liftIO $ sendRes c send inetRessourceID) 5
                            sIDsV <- newMVar []
                            (_,connectLogs) <- runWriterT $ connectToRessource logf c sIDsV (proxyRoadChoice (csources c) sIDsV ) inetRessourceID 
                            logf connectLogs
                            forkIO $ startProxTCP logf tcp_socketName Stream c sIDsV
                            forkIO $ startProxSocks logf Stream c sIDsV
                            udpProx <- newMVar $ newMapModule []
                            void $ forkIO $ startProxUDP logf udpProx c sIDsV 


sendRes :: Client -> SendFunction -> RessourceID -> IO Bool
sendRes c send rID = send $ sendResearch prK pK uID rID 10 [] B.empty
   where (uID, prK, pK) = (keyHash . clientSourceID $ cidentity c, privateKey $ cidentity c, publicKey $ cidentity c) 

seedMain :: LogFunction -> ClientBehaviour 
seedMain logf c send = logf . snd =<< runWriterT (do
                     liftIO $ introduceThread c send "Seed Hello"
                     keepLog TestLog Normal "Enregistrement du callback de ressource"  
                     offerRessourceID c inetRessourceID $ encode "C'est de la bonne"
                     keepLog TestLog Normal "Enregistrement du protocallback UDP"  
                     insertProtoCallback c inetUDPProtoID $ inetUDPProtoCallback logf
                     keepLog TestLog Normal "Enregistrement du protocallback TCP"  
                     insertProtoCallback c inetTCPProtoID $ inetTCPProtoCallback logf c)


relayMain :: ClientBehaviour
relayMain c send = introduceThread c send "Relay hello"

testBinaire :: (ClientBehaviour, LogFunction) -> (ClientBehaviour, LogFunction) -> IO [TestClient]
testBinaire (bh1,l1) (bh2,l2) = newTest [ ([1],bh1,l1), ([0], bh2,l2) ] 


newTest :: [ ([Int], ClientBehaviour, LogFunction) ] -> IO [TestClient]
newTest cL = do --keepLog TestLog Normal "building test"
                tChanL <- atomically $ forM cL $ \c -> newTChan >>= (\tc -> pure (tc, c) )
                gV <- newMVar =<< drgNew
                tcL <- forM tChanL $ genTestC gV $ map fst tChanL
                --keepLog TestLog Normal "done"
                pure tcL
    where genTestC :: MVar RandomGen -> [TChan RawData] -> (TChan RawData, ([Int], ClientBehaviour, LogFunction)) -> IO TestClient
          genTestC gV tChanL (tC,(nL, bhv, logF)) = let send = genSendFun logF nL tChanL in
                                           do (cl,l) <- runWriterT $ newTestClient gV send
                                              logF $ LogMsg Normal TestLog "building testclient" : l
                                              tC' <- atomically $ dupTChan tC
                                              pure $ TestClient cl tC' $ do clM <- runChildren $ runTestClient bhv tC send logF cl
                                                                            readMVar clM
          genSendFun :: LogFunction -> [Int] -> [TChan RawData] -> SendFunction
          genSendFun logf nL tChanL p = do  logf [LogMsg Normal TestLog $ "sending packet..." ++ showPacket p]
                                            forM_ nL $ \i -> (atomically $ writeTChan (tChanL !! i) (encode p))
                                            pure True

keepL = (:[]) . LogMsg Normal TestLog            

runTestClient :: ClientBehaviour -> TChan RawData -> SendFunction -> LogFunction -> Client-> IO ()
runTestClient bhv tc send logf cl = do
        print "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        logf [LogMsg Normal TestLog "qdlkjMDSOVB"]
        logf . keepL $ "Starting timer : "
        tim <- runChildren $ startTimer (pure $ pure ()) (ctimer cl) (1000*1000*1)
        logf . keepL $ "Runing behaviour : "
        bhV <- runChildren $ bhv cl send 
        logf . keepL $ "Starting sniffer : "
        weed <- runChildren $ loop $ listenPacket
        forM_ [weed,tim,bhV] $ readMVar
          where loop f = f >> loop f
                listenPacket = do a <- atomically $ readTChan tc 
                                  case decodeMaybe a :: Maybe Packet of
                                      Nothing -> logf [LogMsg Error TestLog "incorrect Packet received"]
                                      Just pkt -> do logf [LogMsg Normal TestLog $ "received Packet : " ++ showPacket pkt]
                                                     (\m -> logf [LogMsg Normal TestLog m]) . show . fst =<< runStateT dumpClient cl
                                                     (ret,state',logs) <- runRWST onPacket pkt  cl
                                                     logf logs
						     mapM_ send (ret :: [Packet])
						   
                                                     






showPacket :: Packet -> String
showPacket p@(Introduce _ _ _ cnt) = show p ++ " " ++ case decodeMaybe $ runIntroContent cnt :: Maybe NeighHello of
                                                           Just (NeighHello d) -> "NeihHello : " ++ prettyPrint d
                                                           Just NeighClose -> "NeighClose"
                                                           Nothing -> case decodeMaybe $ runIntroContent cnt :: Maybe Request of
                                                                        Just r -> show r
                                                                        Nothing -> "Unreadable Packet"
showPacket p@(DataPacket _ _ cnt) = show p ++ " " ++ case decodeMaybe $ runDataContent cnt :: Maybe RessourcePacket of
                                                    Just rp -> show rp
                                                    Nothing -> case decodeMaybe $ runDataContent cnt :: Maybe PipeMessage of
                                                                Just pm -> show pm ++ case decodeMaybe (messageContent pm) :: Maybe ComMessage of
                                                                                       Just cm -> show cm
                                                                                       Nothing -> "Unreadable MessageContent"
                                                                Nothing -> "Unreadable PipeMessage" 






runChildren :: IO () -> IO (MVar ())
runChildren x = do r <- newEmptyMVar
                   forkFinally x (\err -> print err >> putMVar r ())
                   return r

introduceThread :: (Binary a, Show a) => Client -> SendFunction -> a -> IO ()
introduceThread c send d = do liftIO $ send . genNeighHello (cidentity c) $ encode d
                              repeatEach (ctimer c) (do --liftIO . print $ "sending NeighHello from : " ++ (show . clientSourceID $ cidentity c) ++ "  " ++ (show d)
                                                        liftIO . send  . genNeighHello (cidentity c) $ encode d
                                                        pure ()) 5 >> pure ()



