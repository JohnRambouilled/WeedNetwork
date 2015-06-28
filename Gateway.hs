
module Gateway where

import Protocol
import Packet
import Class
import Sources
import Log
import Client
import Transport.MetaModule

import Control.Monad
import Control.Monad.State hiding (put,get)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy 
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Endian
import Control.Concurrent
import Control.Exception

import Network.Socket hiding (send)
import Network.Socket.ByteString

inetTCPProtoID :: ProtoID
inetTCPProtoID = ProtoID $ encode "www:TCP"
inetUDPProtoID :: ProtoID
inetUDPProtoID = ProtoID $ encode "www:UDP"
localAddr :: IO Word32
localAddr = inet_addr "127.0.0.1"



data InetPacket = InetInit SockConf RawData
                | InetData RawData
                | InetClose RawData

{-| Disjunction between remote and locally stored services. -}
data SockConf = InternetSockConf {scType    :: SocketType,
                                  scAddr    :: Word32,
                                  scDstPort :: Word16}

dumpSockConf str (InternetSockConf scType scaddr scDstPort) = do x <- inet_ntoa scaddr
                                                                 pure $ str ++ " [SOCKCONF] addr=" ++ show x ++ "(" ++ show scaddr ++") : " ++ show scDstPort

inetTCPProtoCallback :: Client -> ProtoCallback
inetTCPProtoCallback c = protoTCPCallback (ctimer c) $ ProtoCallback $ \rd (wr,br) -> do maybe (pure Nothing) (onInetInitPacket Stream wr br) (decodeMaybe rd)

inetUDPProtoCallback :: ProtoCallback
inetUDPProtoCallback = ProtoCallback $ \rd (wr,br) -> do maybe (pure Nothing) (onInetInitPacket Datagram wr br) (decodeMaybe rd)

--onInetInitPacket :: ProtoCallback
--onInetInitPacket = ProtoCallback $ \rd (wr,br) -> do 
--                                                    maybe (pure Nothing) (onInetInitPacket' wr br) (decodeMaybe rd)

onInetInitPacket :: SocketType -> WriteFun -> BreakFun -> InetPacket -> IO (Maybe (Callback, BrkClbck))
onInetInitPacket sctype' wr br (InetInit (InternetSockConf sctype scaddr scdport) raw) | sctype == sctype' = buildGatewayCallback 
                                                                                        | otherwise = pure Nothing
    where buildGatewayCallback = do lcaddr <- localAddr
                                    ("[GATEWAY] connecting to " ++) <$> inet_ntoa scaddr >>= keepLog GatewayLog Normal
                                     >>runDuplexerThread wr br sctype (SockAddrInet (PortNum $ swapEndian scdport) scaddr) raw
                                          
onInetInitPacket _ _ _ _ = return Nothing

{-| Routes from the internet socket to the comID |-}
runDuplexerThread :: WriteFun -> BreakFun -> SocketType -> SockAddr -> RawData -> IO (Maybe (Callback, BrkClbck))
runDuplexerThread wr br socktype sockaddr raw = handle evalException (startDuplexer wr br socktype sockaddr raw >>= uncurry genRet)
    where genRet s pid = pure $ Just (Callback $ gatewayCallback pid br s, BrkClbck $ pure $ killThread pid)
          evalException :: IOException -> IO (Maybe a)
          evalException err = keepLog GatewayLog Error ("[GATEWAY] duplexerThread couldn't be started : " ++ show err) >> return Nothing


startDuplexer :: WriteFun -> BreakFun -> SocketType -> SockAddr -> RawData -> IO (Socket,ThreadId)
startDuplexer wr br socktype sockaddr raw = do keepLog GatewayLog Normal "[GATEWAY] Creating socket"
                                               s <- socket AF_INET socktype defaultProtocol
                                               keepLog GatewayLog Normal "[GATEWAY] Connecting"
                                               connect s sockaddr
                                               keepLog GatewayLog Normal $ "[GATEWAY] Sending payload (" ++ show (Data.ByteString.Lazy.length raw) ++ " bytes)"
                                               when (not $ Data.ByteString.Lazy.null raw) $
                                                       void $ send s $ toStrict raw
                                               keepLog GatewayLog Normal "[GATEWAY] Launching Duplexer"
                                               pid <- forkFinally (runDuplexer True wr br s) (pure $ close s)
                                               keepLog GatewayLog Normal "[GATEWAY] Returning Callbacks"
                                               return (s,pid)


runDuplexer :: Bool -> WriteFun -> BreakFun -> Socket -> IO ()
runDuplexer ignoreInit wr br s = do raw <- fromStrict <$> Network.Socket.ByteString.recv s 4096
                                    keepLog GatewayLog Normal ("[UNIX] New data from a socket (" ++ show (Data.ByteString.Lazy.length raw) ++ ")")
                                    if Data.ByteString.Lazy.null raw then void $ runBreakFun br raw --return ()
                                                                     else do b <- case decodeMaybe raw of
                                                                                Just (InetInit _ _) -> if ignoreInit == True then runWriteFun wr raw
                                                                                                                            else runWriteFun wr (encode $ InetData raw)
                                                                                Nothing             -> runWriteFun wr (encode $ InetData raw)
                                                                             if b then runDuplexer ignoreInit wr br s else return ()




{-| Routes from the comID to the internet socket |-}
gatewayCallback :: ThreadId ->  BreakFun -> Socket -> RawData -> IO ()
gatewayCallback pID br s pkt = keepLog GatewayLog Normal ("[WEED] New data from a comID :"{- ++ BC.unpack pkt-})  >> maybe (pure ()) (gatewayCallback' pID br s) (decodeMaybe pkt)
gatewayCallback' :: ThreadId -> BreakFun -> Socket -> InetPacket -> IO ()
gatewayCallback' pID br s (InetData raw) = do keepLog GatewayLog Normal "[INETDATA]"
                                              r <- send s $ toStrict raw
                                              keepLog GatewayLog Normal $ "[WEED] Writing " ++ show r ++ " bytes into the socket."
gatewayCallback' pID br s (InetClose raw) = do keepLog GatewayLog Normal "[INETCLOSE]"
                                               void $ killThread pID >> keepLog GatewayLog Normal ("[GATEWAY] InetClose received. Closing comID") >> runBreakFun br raw
gatewayCallback' _ _ _ (InetInit _ _) = pure ()







instance Binary SockConf where
        put (InternetSockConf stype daddr dport) = putSockType stype >> putWord32le daddr >> putWord16be dport
            where putSockType Stream = putWord8 0
                  putSockType Datagram = putWord8 1

        get = do InternetSockConf <$> getSockType <*> getWord32le <*> getWord16be

                where getSockType = do v <- getWord8
                                       case v of
                                         0 -> return Stream
                                         1 -> return Datagram
                                         _ -> fail  $ "invalid type of socket " ++ show v ++ " in sock conf (only allowed Stream/Datagram)"
--                                         _ -> return Datagram --fail ""
instance Binary InetPacket where
        put (InetData rd) = putWord8 1 >> putLazyByteString rd
        put (InetClose rd) = putWord8 2 >> putLazyByteString rd
        put (InetInit conf raw) = putWord8 3 >> put conf >> putLazyByteString raw

        get = do w <- getWord8
                 case w of
                   1 -> InetData <$> getRemainingLazyByteString
                   2 -> InetClose <$> getRemainingLazyByteString
                   3 -> InetInit <$> get <*>  getRemainingLazyByteString
                   _ -> fail $ "Not a correct InetPacket : " ++ show w

