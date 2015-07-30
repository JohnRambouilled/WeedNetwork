{-# LANGUAGE PartialTypeSignatures #-}
module Proxy.TCP where

import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as B 
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Data.Binary
import Control.Monad.Writer hiding (listen)
import Data.Maybe

import Client.Packet
import Client
import Client.Sources
import Client.Protocol
import Gateway
import Timer
import Log
import Client.Ressource
import Transport.MetaModule
import Proxy.RoadChoice



data Proxy = Proxy {proxySources :: TVar [SourceEntry]}




{-| Opens a new communication if the socket supplies a well-formed InetInit. |-}
onNewConnection :: LogFunction -> MVar Timer -> MVar Sources -> MVar [SourceID] -> Socket -> IOLog ()
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




runProxy' :: LogFunction -> String -> SocketType -> Client -> MVar [SourceID] -> IOLog Socket
runProxy' lf socketFileName sockType client sIDs = do  --keepLog ProxyLog Normal "PROXY : connectToRessource"
                                                       --connectToRessource lf client sIDs (proxyRoadChoice (csources client) sIDs) inetRessourceID
                                                       keepLog ProxyLog Normal  "PROXY : ouverture de la socket"
                                                       s <- liftIO $ socket AF_UNIX sockType 0
                                                       keepLog ProxyLog Normal "PROXY : bind"
                                                       liftIO $ bindSocket s $ SockAddrUnix socketFileName
                                                       keepLog ProxyLog Normal "PROXY : listen"
                                                       liftIO $ listen s 5 
                                                       keepLog ProxyLog Normal "PROXY : loop"
                                                       pure s

runProxy :: LogFunction -> String -> SocketType -> Client -> MVar [SourceID] -> IO ()
runProxy f fn st c sIDs = do (s,l) <- runWriterT $ runProxy' f fn st c sIDs
                             f l
                             loop $ case st of
                                            Stream -> tcpServer s
                                            Datagram -> return () -- TODO
                                            _ -> return ()
        where loop f = f >> loop f
              tcpServer s = do 
                            (cSock,cAddr) <- accept s
                            f . snd =<< runWriterT (onNewConnection f (ctimer c) (csources c) sIDs cSock)



startProxTCP :: LogFunction -> String -> SocketType -> Client -> MVar [SourceID] -> IO ()
startProxTCP = runProxy 
