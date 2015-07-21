
module Gateway where

import Client.Protocol
import Client.Packet
import Client.Class
import Client.Sources
import Log
import Client
import Transport.MetaModule

import Control.Monad
import Control.Monad.Writer
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

keepL = keepLog GatewayLog

data InetPacket = InetInit SockConf RawData
                | InetData RawData

{-| Disjunction between remote and locally stored services. -}
data SockConf = InternetSockConf {scType    :: SocketType,
                                  scAddr    :: Word32,
                                  scDstPort :: Word16}

dumpSockConf str (InternetSockConf scType scaddr scDstPort) = do x <- inet_ntoa scaddr
                                                                 pure $ str ++ " [SOCKCONF] addr=" ++ show x ++ "(" ++ show scaddr ++") : " ++ show scDstPort

inetTCPProtoCallback :: LogFunction -> Client -> ProtoCallback
inetTCPProtoCallback f c = protoTCPCallback (ctimer c) $ ProtoCallback $ \rd (wr,br) -> do maybe (pure Nothing) (onInetInitPacket f Stream wr br) (decodeMaybe rd)

inetUDPProtoCallback :: LogFunction -> ProtoCallback
inetUDPProtoCallback f = ProtoCallback $ \rd (wr,br) ->  maybe (pure Nothing) (onInetInitPacket f Datagram wr br) (decodeMaybe rd)

--onInetInitPacket :: ProtoCallback
--onInetInitPacket = ProtoCallback $ \rd (wr,br) -> do 
--                                                    maybe (pure Nothing) (onInetInitPacket' wr br) (decodeMaybe rd)

onInetInitPacket :: LogFunction -> SocketType -> WriteFun -> BreakFun -> InetPacket -> IOLog (Maybe (Callback, BrkClbck))
onInetInitPacket f sctype' wr br (InetInit (InternetSockConf sctype scaddr scdport) raw) | sctype == sctype' = buildGatewayCallback 
                                                                                        | otherwise = pure Nothing
    where buildGatewayCallback = do lcaddr <- liftIO localAddr
                                    ("[GATEWAY] connecting to " ++) <$> liftIO (inet_ntoa scaddr) >>= keepLog GatewayLog Normal
                                    runDuplexerThread f wr br sctype (SockAddrInet (PortNum $ swapEndian scdport) scaddr) raw
                                          
onInetInitPacket _ _ _ _ _ = return Nothing

{-| Routes from the internet socket to the comID |-}
runDuplexerThread :: LogFunction -> WriteFun -> BreakFun -> SocketType -> SockAddr -> RawData -> IOLog (Maybe (Callback, BrkClbck))
runDuplexerThread f wr br socktype sockaddr raw = handle' evalException (startDuplexer f wr br socktype sockaddr raw >>= uncurry genRet)
    where genRet s pid = pure $ Just (Callback $ gatewayCallback pid br s, BrkClbck $ pure $ liftIO $ killThread pid)
          handle' f a = do (r,w) <- liftIO $ handle (runWriterT . f) (runWriterT a)
                           tell w >> pure r
          evalException :: IOException -> IOLog (Maybe a)
          evalException err = keepL Error ("[GATEWAY] duplexerThread couldn't be started : " ++ show err) >> return Nothing


startDuplexer :: LogFunction -> WriteFun -> BreakFun -> SocketType -> SockAddr -> RawData -> IOLog (Socket,ThreadId)
startDuplexer f wr br socktype sockaddr raw = do keepLog GatewayLog Normal "[GATEWAY] Creating socket"
                                                 s <- liftIO $ socket AF_INET socktype defaultProtocol
                                                 keepLog GatewayLog Normal "[GATEWAY] Connecting"
                                                 liftIO $ connect s sockaddr
                                                 keepLog GatewayLog Normal $ "[GATEWAY] Sending payload (" ++ show (Data.ByteString.Lazy.length raw) ++ " bytes)"
                                                 when (not $ Data.ByteString.Lazy.null raw) $
                                                       void . liftIO $ send s $ toStrict raw
                                                 keepLog GatewayLog Normal "[GATEWAY] Launching Duplexer"
                                                 pid <- liftIO $ forkFinally (runDuplexer f True wr br s) (pure $ close s)
                                                 keepLog GatewayLog Normal "[GATEWAY] Returning Callbacks"
                                                 return (s,pid)

runDuplexer :: LogFunction -> Bool -> WriteFun -> BreakFun -> Socket -> IO ()
runDuplexer f ignoreInit wr br s =  do raw <- fromStrict <$> Network.Socket.ByteString.recv s 4096
                                       (c,l) <- runWriterT $ runDuplexer' raw
                                       f l
                                       if c then runDuplexer f ignoreInit wr br s else return ()
  where runDuplexer' :: RawData -> IOLog Bool
        runDuplexer' raw = do keepLog GatewayLog Normal ("[UNIX] New data from a socket (" ++ show (Data.ByteString.Lazy.length raw) ++ ")")
                              if Data.ByteString.Lazy.null raw then runBreakFun br raw >> pure False
                                                               else case decodeMaybe raw of
                                                                    Just (InetInit _ _) -> if ignoreInit == False then runWriteFun wr raw
                                                                                                                else runWriteFun wr (encode $ InetData raw)
                                                                    Nothing             -> runWriteFun wr (encode $ InetData raw)
                                                                                




{-| Routes from the comID to the internet socket |-}
gatewayCallback :: ThreadId ->  BreakFun -> Socket -> RawData -> IOLog ()
gatewayCallback pID br s pkt = do keepLog GatewayLog Normal ("[WEED] New data from a comID :"{- ++ BC.unpack pkt-}) 
                                  case decodeOrFail pkt of
                                            Left (_, _ ,e) -> keepL Error $ "fail to decode InetPacket : " ++ e
                                            Right (_, _, r) -> gatewayCallback' pID br s r 
gatewayCallback' :: ThreadId -> BreakFun -> Socket -> InetPacket -> IOLog ()
gatewayCallback' pID br s (InetData raw) = do keepLog GatewayLog Normal "[INETDATA]"
                                              liftIO $ Prelude.putStrLn "[PRINTF_ GATEWAY] inet data received"
                                              r <- handle' evalException (liftIO $ void $ send s $ toStrict raw)
                                              keepLog GatewayLog Normal $ "[WEED] Writing " ++ show r ++ " bytes into the socket."
    where handle' f a = do (r,w) <- liftIO $ handle (runWriterT . f) (runWriterT a)
                           tell w >> pure r
          evalException :: IOException -> IOLog ()
          evalException err = keepL Error ("!!!!!!!!!!!!!!!!!!!!!![DUPLEXER] Send has failed" ++ show err) >> void (runBreakFun br BC.empty)

{-gatewayCallback' pID br s (InetClose raw) = do keepLog GatewayLog Normal "[INETCLOSE]"
                                               liftIO $ killThread pID 
                                               keepLog GatewayLog Normal ("[GATEWAY] InetClose received. Closing comID") 
                                               --void $ runBreakFun br raw
-}
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
 --       put (InetClose rd) = putWord8 2 >> putLazyByteString rd
        put (InetInit conf raw) = putWord8 3 >> put conf >> putLazyByteString raw

        get = do w <- getWord8
                 case w of
                   1 -> InetData <$> getRemainingLazyByteString
--                   2 -> InetClose <$> getRemainingLazyByteString
                   3 -> InetInit <$> get <*>  getRemainingLazyByteString
                   _ -> fail $ "Not a correct InetPacket : " ++ show w

