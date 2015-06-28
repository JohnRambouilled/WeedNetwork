{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Proxy.UDP where

import Class
import Protocol
import Packet
import Gateway
import Proxy
import Timer
import Sources
import Client
import Log
--import Proxifier.UDP

import Data.Maybe
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Lazy as B
import Control.Monad.State hiding (get,put)
import qualified Control.Monad.State  as S (get)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.MVar

udpTimeOut = 120 --TODO
udpSocketFile = "testounet_udp.socket"

data ProxUDPEntry = ProxUDPEntry {proxUDPWrite :: WriteFun,
                                  proxUDPBreak :: BreakFun,

                                  proxRefresh :: IO (),
                                  proxKill    :: IO (),
                                  
                                  proxSockConf :: SockConf}
newtype SocketFile = SocketFile String
  deriving (Eq,Ord, Show)
data ProxRet = ProxRet

data ProxPacket = ProxPacket {proxSource :: SockAddr,
                              proxData :: RawData}

data ProxAns = ProxAns {ansSource :: SockConf,
                        ansData :: RawData}



addrToFilename (SockAddrUnix x) = SocketFile x
addrToFilename _ = error "addrToFilename() : wrong type of structure"

type ProxUDP = MapModule ProxUDPEntry SocketFile ProxPacket ProxRet

{-| Writes everything into the pipe, according to the source specified in the hashmap |-}
instance MapModules ProxUDPEntry SocketFile ProxPacket ProxRet where
        packetKey (ProxPacket (SockAddrUnix sockfile) _) = do keepLog ProxyLog Normal $ "new packet from sock : " ++ sockfile
                                                              S.get >>= keepLog ProxyLog Normal . show
                                                              pure $ Just $ SocketFile sockfile
        entryBehaviour _ = [\entry packet -> callback entry packet >> pure [ProxRet]]
          where   callback :: ProxUDPEntry -> ProxPacket -> StateT ProxUDP IO () 
                  callback entry packet 
                    -- The socket has failed to read
                    |B.null (proxData packet) = liftIO $ proxKill entry 
                    -- The data has to be checked by the layer before sending
                    |otherwise                = void $ buildProxCallback (proxUDPWrite entry) (proxUDPBreak entry)  (proxRefresh entry) (proxKill entry)  packet
                            

runProxUDP :: (MonadIO m) => MVar ProxUDP -> Client  -> StateT ProxUDP m Socket
runProxUDP proxyV client = do sIDsV <- liftIO $ atomically $ newTVar []
                              liftIO $ connectToRessource client sIDsV (proxyRoadChoice (csources client) sIDsV) inetRessourceID
                              s <- liftIO initSock 
                              modifyDefaultMapBehaviour $ \l -> behaviour proxyV (csources client) (ctimer client) s sIDsV:l
                              return s
        where initSock = do s <- socket AF_UNIX Datagram 0
                            bindSocket s $ SockAddrUnix udpSocketFile 
--                            c_set_block $ fdSocket s
                            return s

startProxUDP :: MVar ProxUDP -> Client -> IO ()
startProxUDP proxyV client = do s <- runStateMVar proxyV (runProxUDP proxyV client)
                                withSocketsDo $ loop $ runServer s
  where runServer :: Socket -> IO [ProxRet]
        runServer s = do keepLog ProxyLog Normal "waiting for udp packet..."
                         (pkt,caddr@(SockAddrUnix fname)) <- recvFrom s 4096
                         keepLog ProxyLog Normal $ "[UDP PROXY] new udp packet from " ++ fname  ++ " : " ++ show pkt
                         runModule proxyV $ ProxPacket caddr (B.fromStrict pkt)
        loop f = f >> loop f
                                                                                
                              



behaviour :: MVar ProxUDP -> MVar Sources -> MVar Timer -> Socket -> TVar [SourceID] -> MapBehaviour ProxUDPEntry SocketFile ProxPacket ProxRet
behaviour proxyV sourcesV timerV sock sIDsV p@(ProxPacket src raw) = do keepLog ProxyLog Normal "Calling default behaviour"
                                                                        case decodeOrFail raw of
                                                                          Left (_,_,msg) -> do keepLog ProxyLog Error $ "unable to parse InetPacket : " ++ msg ++ " (" ++ show (B.length raw) ++ " bytes)"
                                                                                               keepLog ProxyLog Error $ show raw 
                                                                                               pure [] 
                                                                          Right (_,_,inetPkt) -> registerCB inetPkt
  where registerCB :: (MonadIO m) => InetPacket -> MapModuleT ProxUDPEntry SocketFile ProxPacket ProxRet m [ProxRet]
        registerCB (InetInit sc initMsg) = do keepLog ProxyLog Normal "chosing destinary"
                                              dest <- liftIO $ choseDest sourcesV sIDsV
                                              keepLog ProxyLog Normal  $ "[UDP PROXY] New InetInit !"
                                              let sockfile = addrToFilename src
                                              if isNothing dest then do keepLog ProxyLog Error $ "[UDP PROXY] No roads available..."
                                                                        pure []
                                                                else do (refresh,killTO) <- liftIO $ registerTimerM timerV udpTimeOut (unregisterProxySocket sockfile >> return True)
--                                                                        (refresh,killTO) <- liftIO $ registerTimerM timerV udpTimeOut (unregisterProxySocket sockfile >> return True)
                                                                        let killTO' = unregisterM proxyV sockfile >> killTO
                                                                        (wr,br) <- liftIO $ openCommunicationTO (buildPipeCallback sock src killTO' sc)
                                                                                                             timerV leechTimeOut leechRefreshTime
                                                                                                             (fromJust dest) $ ProtoRequest inetUDPProtoID raw
                                                                        insertMapBehaviour sockfile $ ProxUDPEntry wr br refresh killTO' sc
                                                                        keepLog ProxyLog Normal $ "[UDP PROXY] New communication openned."
                                                                        return []
        registerCB _ = return []
        -- TODO l'entrée est clean deux fois si timedout
        unregisterProxySocket sockfile = runStateMVar proxyV $ do entry <- mapGetEntry sockfile
                                                                  let sfile (SocketFile f) = f
                                                                  keepLog ProxyLog Normal $ "[UDP PROXY] Entry timed out, CLOSING  " ++ (sfile sockfile)
                                                                  when (isJust entry) $ do
                                                                          liftIO $ runBreakFun (proxUDPBreak $ fromJust entry) $ B.empty
                                                                          removeMapBehaviour sockfile
                                                                  when (isNothing entry) $ keepLog ProxyLog Error $ "[UDP PROXY] Key not found :" ++ sfile sockfile



writeData :: WriteFun -> BreakFun -> IO () -> RawData -> IO [ProxRet]
writeData wr br kill rd = do r <- runWriteFun wr rd
                             if r then pure [] else runBreakFun br B.empty >> kill >> pure []


{-| Writes everything from the socket into the pipe. InetInit may refresh the connection [TODO] control with hijacking sendto in the proxifier |-}
buildProxCallback :: WriteFun -> BreakFun ->  IO () -> IO () -> MapBehaviour ProxUDPEntry SocketFile ProxPacket ProxRet
buildProxCallback wr br refresh kill (ProxPacket src raw) = case decodeMaybe raw of
                                                                           Just (InetInit _ _ ) -> -- liftIO $ refresh >>  [TODO] No reason to refresh on arbitrary packets
                                                                                                  do keepLog ProxyLog Normal $ ("[UDP PROXY] InetInit received " ++ show (B.length raw) ++ " bytes from the socket. Writing into weed.") 
                                                                                                     liftIO $ writeData wr br kill (encode $ InetData raw)
                                                                           _ -> keepLog ProxyLog Normal ("[UDP PROXY] received " ++ show (B.length raw) ++ " bytes from the socket. Writing into weed.") 
                                                                                >> liftIO (writeData wr br kill (encode $ InetData raw ))
                                                                                       

{-| Writes everything from the pipe into the udp socket |-}
buildPipeCallback :: Socket -> SockAddr -> IO () -> SockConf -> (WriteFun,BreakFun) -> IO (Callback,BrkClbck)
buildPipeCallback sock addr kill sc (wr,br) = pure (Callback normalCallback, BrkClbck closeCallback)
  where normalCallback pkt = case decodeMaybe pkt of
                                Nothing -> return ()
                                Just (InetInit _ rd) -> return () --TODO
                                Just (InetData rd) -> do 
                                                        -- ret <- sendTo sock (B.toStrict rd) addr
                                                         ret <- sendTo sock (B.toStrict $ encode $ ProxAns sc rd) addr
                                                         keepLog ProxyLog Normal $ "[UDP PROXY] New data from weed to socket " ++ show addr ++" (" ++ show ret ++ " bytes) - " ++ show ((fromIntegral $ B.length rd)::Word64)
                                Just (InetClose rd) -> void $ --sendTo sock (B.toStrict rd) addr >>  
                                                             runBreakFun br B.empty >> kill

        closeCallback _ = kill


instance Binary ProxAns where
--        put (ProxAns sc raw) = put sc >> putLazyByteString raw
        put (ProxAns sc raw) = put sc >> put ((fromIntegral $ B.length raw)::Word64) >> putLazyByteString raw
        get = do sc <- get 
                 siz <- fromIntegral <$> (get :: Get Word64)
                 raw <- getLazyByteString siz -- $ fromIntegral siz
                 return $ ProxAns  sc raw
