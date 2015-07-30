module Proxy.Socks where

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as B 
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
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
import Proxy.RoadChoice

tcpSocksPort = 1080 ::Word16

data Proxy = Proxy {proxySources :: MVar [SourceEntry]}

{-| Opens a new communication if the socket supplies a well-formed InetInit. |-}
onNewConnection :: LogFunction -> MVar Timer -> MVar Sources -> MVar [SourceID] -> Socket -> IOLog ()
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
                                   
        where
              buildCallbacks (wr,br) = do pID <- liftIO $ forkFinally (runDuplexer logf True wr br s) (pure $ close s) 
                                          return (Callback $ gatewayCallback pID br s, BrkClbck $ \_ -> liftIO $  killThread pID)




runProxy :: LogFunction -> SocketType -> Client -> MVar [SourceID] -> IOLog Socket
runProxy logf sockType client sIDs = do  --keepLog ProxyLog Normal "PROXY : connectToRessource"
                                         --connectToRessource logf client sIDs (proxyRoadChoice (csources client) sIDs) inetRessourceID
                                         keepLog ProxyLog Normal  "PROXY : ouverture de la socket"
                                         s <- liftIO $ socket AF_INET sockType 0
                                         liftIO $ setSocketOption s ReuseAddr 1
                                         keepLog ProxyLog Normal "PROXY : bind"
                                         liftIO $ bind s (SockAddrInet (PortNum $ swapEndian 1337) iNADDR_ANY)
                                         keepLog ProxyLog Normal "PROXY : listen"
                                         liftIO $ listen s 5 
                                         keepLog ProxyLog Normal "PROXY : loop"
                                         pure s



startProxSocks :: LogFunction -> SocketType -> Client -> MVar [SourceID] -> IO ()
startProxSocks logf sockType client sIDs = do
                                         (s,logs) <- runWriterT (runProxy logf sockType client sIDs)
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

