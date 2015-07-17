{-# LANGUAGE PartialTypeSignatures #-}
module Proxy where

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as B 
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.State
import Data.Binary
import Control.Monad
import Control.Monad.Writer hiding (listen)
import Data.Maybe
import Data.List 

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


data Proxy = Proxy {proxySources :: TVar [SourceEntry]}


{-| Always keeps k roads to the internet |-}
proxyRoadChoice :: MVar Sources -> TVar [SourceID] -> RoadChoice
proxyRoadChoice sourcesV sIDsV  = RoadChoice roadC
  where roadC road sID cert raw = do 
                                     sIDs <- liftIO . atomically $ readTVar sIDsV
                                     keepLog ProxyLog Normal $ "[Roads] : RoadChoice on road : " ++ show (roadToRoadID road)
                                     keepLog ProxyLog Normal $ "[Roads] : Registered roads are : " ++ show sIDs
                                     if length sIDs < nbSourcesMax 
                                         then case sID `elem` sIDs of
                                                False -> pure True
                                                True -> extractRoads sourcesV sID >>= maybe (pure False) (pure . (roadToRoadID road `notElem`))
                                         else return False

                                                                                
choseDest :: MVar Sources -> TVar [SourceID] -> IOLog (Maybe SourceEntry)
choseDest sourcesV sIDs = (liftIO . atomically $ safeHead <$> readTVar sIDs) >>= maybe (pure Nothing) (getSourceEntry sourcesV)  
  where safeHead [] = Nothing
        safeHead (x:_) = Just x

{-

{-| Opens a new communication if the socket supplies a well-formed InetInit. |-}
onNewConnection :: LogFunction -> MVar Timer -> MVar Sources -> TVar [SourceID] -> Socket -> IOLog ()
onNewConnection f timerV sourcesV sIDs s = do 
                                     keepLog ProxyLog Normal "NEW CONNECTION !!!"
                                     raw <- liftIO $ recv s 4096
                                     keepLog ProxyLog Normal $ "[PROXY] received " ++ show (BS.length raw) ++ " bytes."
                                     case decodeMaybe (B.fromStrict raw) :: Maybe InetPacket of
                                        Just pkt@(InetInit sc _) -> do
                                                                      keepLog ProxyLog Normal =<< liftIO (dumpSockConf "[UNIX] new pkt decoded" sc)
                                                                      dest <- choseDest sourcesV sIDs
                                                                      if (isNothing dest) then liftIO $ close s
                                                                        else void $ openTCPCommunication buildCallbacks timerV leechTimeOut leechRefreshTime
                                                                                                      (fromJust dest) $ ProtoRequest inetTCPProtoID (encode pkt)
                                        _ -> do --case decodeOrFail (B.fromStrict raw) of
                                               --     Right (_,_,a) -> do pkt <- pure a :: IO InetPacket
                                               --                         close s
                                               --     Left (_,_,msg) -> do keepLog ProxyLog Error $ "erreur decode packet : " ++ msg
                                                liftIO $ close s
        where
              buildCallbacks (wr,br) = do pID <- liftIO $ forkFinally (runDuplexer f False wr br s) (pure $ close s) 
                                          return (Callback $ gatewayCallback pID br s, BrkClbck $ \_ -> (liftIO $ killThread pID))




runProxy' :: String -> SocketType -> Client -> TVar [SourceID] -> IOLog Socket
runProxy' socketFileName sockType client sIDs = do  keepLog ProxyLog Normal "PROXY : connectToRessource"
                                                    connectToRessource client sIDs (proxyRoadChoice (csources client) sIDs) inetRessourceID
                                                    keepLog ProxyLog Normal  "PROXY : ouverture de la socket"
                                                    s <- liftIO $ socket AF_UNIX sockType 0
                                                    keepLog ProxyLog Normal "PROXY : bind"
                                                    liftIO $ bindSocket s $ SockAddrUnix socketFileName
                                                    keepLog ProxyLog Normal "PROXY : listen"
                                                    liftIO $ listen s 5 
                                                    keepLog ProxyLog Normal "PROXY : loop"
                                                    pure s

runProxy :: LogFunction -> String -> SocketType -> Client -> TVar [SourceID] -> IO ()
runProxy f fn st c sIDs = do (s,l) <- runWriterT $ runProxy' fn st c sIDs
                             f l
                             loop $ case st of
                                            Stream -> tcpServer s
                                            Datagram -> return () -- TODO
                                            _ -> return ()
        where loop f = f >> loop f
              tcpServer s = do 
                            (cSock,cAddr) <- accept s
                            f . snd =<< runWriterT (onNewConnection f (ctimer c) (csources c) sIDs cSock)



startProxy :: LogFunction -> String -> SocketType -> Client -> IO ()
startProxy f socketFileName sockType client = atomically (newTVar []) >>= runProxy f socketFileName sockType client -}
