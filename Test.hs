{-# LANGUAGE FlexibleContexts #-}
module Test where

import Control.Monad.State
import Control.Concurrent
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.List
import Data.Binary
import System.Random (randomIO)

import Client.Class
import Client.Crypto
import Client.Packet
import Client.Routing
import Client.Sources
import Client.Ressource
import Client.Neighborhood
import Proxy
import Proxy.UDP
import Gateway
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




newTestClient ::  MVar RandomGen -> SendFunction -> IO Client
newTestClient gV send = do (pubkey,privkey) <- genKeyPairMVar gV
                           keepLog TestLog Normal "Creating client"
                           let me = SourceID $ KeyHash $ pubKeyToHash pubkey
                           genClient gV me privkey pubkey send



type ClientBehaviour = Client -> SendFunction -> IO ()



testFullGraph :: [ClientBehaviour] -> IO [TestClient]
testFullGraph cL = newTest $ zipWith genNeigh [0..] cL
    where genNeigh i bH = (filter (/=i) [0.. (length cL - 1)], bH)


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
          



testChaine :: [ClientBehaviour] -> IO [TestClient]
testChaine cL = newTest $ zipWith genNeigh [0..] cL
    where genNeigh i bh = if i == 0 then ([1], bh)
                                    else if i + 1 == length cL then ([i - 1], bh)
                                                               else ([i - 1, i + 1], bh)



tcp_socketName = "testounet_tcp.socket"
leechMain :: ClientBehaviour  
leechMain c send = do introduceThread c send "Leech Hello"
                      keepLog TestLog Normal "enregistrement de la répétition de recherche"
                      keepLog TestLog Normal "démarrage du proxy "
                      repeatEach (ctimer c) (void $ sendRes c send inetRessourceID) 5
                      forkIO $ startProxy tcp_socketName Stream c
                      udpProx <- newMVar $ newMapModule []
                      void $ forkIO $ startProxUDP udpProx c


sendRes :: Client -> SendFunction -> RessourceID -> IO Bool
sendRes c send rID = send $ sendResearch prK pK uID rID 10 [] B.empty
   where (uID, prK, pK) = (keyHash . clientSourceID $ cidentity c, privateKey $ cidentity c, publicKey $ cidentity c) 

seedMain :: ClientBehaviour 
seedMain c send = do introduceThread c send "Seed Hello"
                     keepLog TestLog Normal "Enregistrement du callback de ressource"  
                     offerRessourceID c inetRessourceID $ encode "C'est de la bonne"
                     keepLog TestLog Normal "Enregistrement du protocallback UDP"  
                     insertProtoCallback c inetUDPProtoID inetUDPProtoCallback
                     keepLog TestLog Normal "Enregistrement du protocallback TCP"  
                     insertProtoCallback c inetTCPProtoID $ inetTCPProtoCallback c


relayMain :: ClientBehaviour
relayMain c send = introduceThread c send "Relay hello"

testBinaire :: ClientBehaviour -> ClientBehaviour -> IO [TestClient]
testBinaire bh1 bh2 = newTest [ ([1],bh1), ([0], bh2) ] 


newTest :: [ ([Int], ClientBehaviour) ] -> IO [TestClient]
newTest cL = do keepLog TestLog Normal "building test"
                tChanL <- atomically $ forM cL $ \c -> newTChan >>= (\tc -> pure (tc, c) )
                gV <- newMVar =<< drgNew
                tcL <- forM tChanL $ genTestC gV $ map fst tChanL
                keepLog TestLog Normal "done"
                pure tcL
    where genTestC :: MVar RandomGen -> [TChan RawData] -> (TChan RawData, ([Int], ClientBehaviour)) -> IO TestClient
          genTestC gV tChanL (tC,(nL, bhv)) = let send = genSendFun nL tChanL in
                                           do cl <- newTestClient gV send
                                              keepLog TestLog Normal "building testclient"
                                              tC' <- atomically $ dupTChan tC
                                              pure $ TestClient cl tC' $ do clM <- runChildren $ runTestClient bhv tC send cl
                                                                            readMVar clM
          genSendFun :: [Int] -> [TChan RawData] -> SendFunction
          genSendFun nL tChanL p = do keepLog TestLog Normal $ "sending packet..." ++ showPacket p
                                      forM_ nL $ \i -> (atomically $ writeTChan (tChanL !! i) (encode p))
                                      pure True
            
runTestClient :: ClientBehaviour -> TChan RawData -> SendFunction -> Client-> IO ()
runTestClient bhv tc send cl = do
        tim <- runChildren $ startTimer (ctimer cl) (1000*1000*1)
        bhV <- runChildren $ bhv cl send 
        weed <- runChildren $ loop $ listenPacket
        forM_ [weed,tim,bhV] $ readMVar
          where loop f = f >> loop f
                listenPacket = do a <- atomically $ readTChan tc 
                                  case decodeMaybe a :: Maybe Packet of
                                      Nothing -> keepLog TestLog Error "incorrect Packet received"
                                      Just pkt -> do keepLog TestLog Normal $ "received Packet : " ++ showPacket pkt
                                                     keepLog TestLog Normal . fst =<< runStateT dumpClient cl
                                                     fst <$> runStateT (onPacket pkt) cl >>= mapM_ send              
                                                     






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
introduceThread c send d = do send . genNeighHello (cidentity c) $ encode d
                              repeatEach (ctimer c) (do print $ "sending NeighHello from : " ++ (show . clientSourceID $ cidentity c) ++ "  " ++ (show d)
                                                        send  . genNeighHello (cidentity c) $ encode d
                                                        pure ()) 5 >> pure ()



