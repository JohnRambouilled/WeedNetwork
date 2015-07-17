{-# LANGUAGE PartialTypeSignatures #-}
module Proxy.TCP where

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as B 
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.State
import Control.Monad.Writer hiding (listen)
import Data.Binary
import Control.Monad
import Data.Maybe
import Data.List 
import Data.Endian

import Client.Class
import Client.Packet
import Client
import Client.Sources
import Client.Communication
import Client.Protocol
import Client.Pipes
import Gateway
import Timer
import Log
import Client.Ressource
import Transport.MetaModule

nbRoads = 1
nbSourcesMax = 1
inetRessourceID :: RessourceID
inetRessourceID = RessourceID $ encode "www"
tcpSocksPort = 1080


data Proxy = Proxy {proxySources :: TVar [SourceEntry]}


{-| Always keeps k roads to the internet |-}
proxyRoadChoice :: MVar Sources -> TVar [SourceID] -> RoadChoice
proxyRoadChoice sourcesV sIDsV  = RoadChoice roadC
  where roadC road sID cert raw = do 
                                     sIDs <- liftIO $ atomically $ readTVar sIDsV
                                     keepLog ProxyLog Normal $ "[Roads] : RoadChoice on road : " ++ show (roadToRoadID road)
                                     keepLog ProxyLog Normal $ "[Roads] : Registered roads are : " ++ show sIDs
                                     if length sIDs < nbSourcesMax 
                                         then case sID `elem` sIDs of
                                                False -> pure True
                                                True -> extractRoads sID >>= maybe (pure False) (pure . (roadToRoadID road `notElem`))
                                         else return False
        extractRoads :: SourceID -> IOLog (Maybe [RoadID])
        extractRoads sID = do 
--                              pipes <- (sourcePipes . fromJust) <$> getSourceEntry sourcesV sID   
                              pipes <- getSourceEntry sourcesV sID >>= maybe (pure Nothing) (pure . Just . sourcePipes)  
                              if isNothing pipes then return Nothing
                                                 else liftIO $ (Just . pipesList) <$> readMVar (fromJust pipes)


                                                                                
choseDest :: MVar Sources -> TVar [SourceID] -> IOLog (Maybe SourceEntry)
choseDest sourcesV sIDs = (liftIO $ atomically $ safeHead <$> readTVar sIDs) >>= maybe (pure Nothing) (getSourceEntry sourcesV)  
  where safeHead [] = Nothing
        safeHead (x:_) = Just x


{-| Opens a new communication if the socket supplies a well-formed InetInit. |-}
onNewConnection :: LogFunction -> MVar Timer -> MVar Sources -> TVar [SourceID] -> Socket -> IOLog ()
onNewConnection logf timerV sourcesV sIDs s = do 
                                     keepLog ProxyLog Normal "NEW CONNECTION !!!"
--                                     raw <- recv s 4096
--                                     keepLog ProxyLog Normal $ "[PROXY] received " ++ show (BS.length raw) ++ " bytes."

--                                     keepLog ProxyLog Normal =<< dumpSockConf "[UNIX] new pkt decoded" sc
                                     dest <- choseDest sourcesV sIDs
                                     addr <- liftIO $ localAddr
                                     let pkt = InetInit (InternetSockConf Stream addr tcpSocksPort) B.empty
                                     if (isNothing dest) then liftIO $ close s
                                                          else void $ openTCPCommunication buildCallbacks timerV leechTimeOut leechRefreshTime
                                                                                                      (fromJust dest) $ ProtoRequest inetTCPProtoID (encode pkt)
                                   
--                                     case decodeMaybe (B.fromStrict raw) :: Maybe InetPacket of
--                                        Just pkt@(InetInit sc _) -> do
                                               
--                                                                      keepLog ProxyLog Normal =<< dumpSockConf "[UNIX] new pkt decoded" sc
--                                                                      dest <- choseDest sourcesV sIDs
--                                                                      if (isNothing dest) then close s
--                                                                        else void $ openTCPCommunication buildCallbacks timerV leechTimeOut leechRefreshTime
--                                                                                                      (fromJust dest) $ ProtoRequest inetTCPProtoID (encode pkt)
--                                        _ -> do --case decodeOrFail (B.fromStrict raw) of
                                               --     Right (_,_,a) -> do pkt <- pure a :: IO InetPacket
                                               --                         close s
                                               --     Left (_,_,msg) -> do keepLog ProxyLog Error $ "erreur decode packet : " ++ msg
--                                              close s
        where
              buildCallbacks (wr,br) = do pID <- liftIO $ forkFinally (runDuplexer logf True wr br s) (pure $ close s) 
                                          return (Callback $ gatewayCallback pID br s, BrkClbck $ \_ -> liftIO $  killThread pID)




runProxy :: LogFunction -> String -> SocketType -> Client -> TVar [SourceID] -> IOLog Socket
runProxy logf socketFileName sockType client sIDs = do  keepLog ProxyLog Normal "PROXY : connectToRessource"
                                                        connectToRessource logf client sIDs (proxyRoadChoice (csources client) sIDs) inetRessourceID
                                                        keepLog ProxyLog Normal  "PROXY : ouverture de la socket"
                                                        s <- liftIO $ socket AF_INET sockType 0
                                                        liftIO $ setSocketOption s ReuseAddr 1
                                                        keepLog ProxyLog Normal "PROXY : bind"
                                                        liftIO $ bind s (SockAddrInet (PortNum $ swapEndian 1337) iNADDR_ANY)
                                                        keepLog ProxyLog Normal "PROXY : listen"
                                                        liftIO $ listen s 5 
                                                        keepLog ProxyLog Normal "PROXY : loop"
                                                        pure s



startProxy :: LogFunction -> String -> SocketType -> Client -> IO ()
startProxy logf socketFileName sockType client = do sIDs <- atomically $ newTVar []
                                                    (s,logs) <- runWriterT (runProxy logf socketFileName sockType client sIDs)
                                                    logf logs
                                                    loop $ case sockType of
                                                                    Stream -> tcpServer s sIDs
                                                                    Datagram -> return () -- TODO
                                                                    _ -> return ()
        where loop f = f >> loop f
              tcpServer s sIDs = do 
                            (cSock,cAddr) <- accept s
                            (_,logs) <- runWriterT $ onNewConnection logf (ctimer client) (csources client) sIDs cSock
                            logf logs

