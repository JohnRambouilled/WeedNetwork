{-# LANGUAGE ForeignFunctionInterface #-}
module Sniffer.Ethernet where
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Data.Binary
import Data.Maybe
import Control.Concurrent
import System.Posix.Signals
import Foreign.ForeignPtr

import Client.Packet

type Socket = CInt

foreign import ccall "ethernet.h write_pkt"
    c_write_pkt :: CInt -> Ptr () -> CString -> CUInt -> IO CInt
foreign import ccall "ethernet.h new_sin"
    c_new_sin :: CInt -> IO (Ptr ())

foreign import ccall "receiver.h &delete_sin"
    c_delete_sin :: FunPtr (Ptr () -> IO ())


foreign import ccall "receiver.h getSock"
    c_getSock :: IO CInt
foreign import ccall "receiver.h closeSock"
    c_closeSock :: CInt -> IO ()
foreign import ccall "receiver.h getBuf"
    c_getBuf :: Ptr CChar -> IO (Ptr CChar)
foreign import ccall "receiver.h newBuf"
    c_newBuf :: IO (Ptr CChar)
foreign import ccall "receiver.h &delBuf"
    c_delBuf :: FunPtr (Ptr CChar -> IO ())

foreign import ccall "receiver.h next_pkt"
    c_next_pkt :: CInt -> Ptr () -> Ptr CChar -> IO CInt
--foreign import ccall unsafe "receiver.h next_pkt_timeout"
--    c_next_pkt_timeout :: CInt -> CInt -> IO CInt

data RawSocket = RawSocket {rawSock :: CInt,
                            rawConf :: ForeignPtr (),
                            rawBuf  :: ForeignPtr CChar}


newRawSocket :: IO RawSocket
newRawSocket = do s <- c_getSock
                  conf <- c_new_sin s >>= newForeignPtr c_delete_sin 
                  buf <- c_newBuf >>= newForeignPtr c_delBuf
                  return $ RawSocket s conf buf

closeRawSocket :: RawSocket -> IO ()
closeRawSocket = c_closeSock . rawSock



-- Envoie une bytestring sur le réseau (sans le header ethernet)
sendPkt :: RawSocket -> ByteString -> IO Bool
sendPkt sock raw = useAsCStringLen raw $ \(str,siz) ->
        do --print $ "sending : " ++ show raw
           ret <- withForeignPtr (rawConf sock) $ \conf -> 
                    c_write_pkt (rawSock sock) conf str (fromIntegral siz)
           if 0 == fromIntegral ret then return True else return False
-- Recoit une bytestring du réseau (sans le header ethernet)
recvPkt :: RawSocket -> IO (Maybe ByteString)
recvPkt sock = do siz <- withForeignPtr (rawBuf sock) $ \buf ->
                        withForeignPtr (rawConf sock) $ \conf -> 
                            c_next_pkt (rawSock sock) conf buf
--                  Prelude.putStrLn $ "haskell : " ++ show siz
                  if siz == 0 then return Nothing
                  else withForeignPtr (rawBuf sock) $ \buf -> do buf' <- c_getBuf buf
                                                                 Just <$> packCStringLen (buf',fromIntegral siz - 12)


{-| Returns Nothing if the socket has failed. |-}
waitForPkt :: RawSocket -> IO (Maybe Packet)
waitForPkt sock  = do raw <- recvPkt sock
                      if isNothing raw then --Prelude.putStrLn "Nothing received" >>
                                             error "The raw socket has failed to read the network layer... closing"
                      else --Prelude.print raw >> 
                           --case decodeMaybe $ L.fromStrict (fromJust raw) of
                           --       Nothing -> -- Prelude.putStrLn ("cannot decode ") >>
                           --                  (return Nothing)
                            --      Just pkt -> -- Prelude.putStrLn" decode" >> 
                            --                 (return $Just pkt)
                            case decodeOrFail $ L.fromStrict (fromJust raw) of
                              Left err -> return Nothing --print err >> return Nothing
                              Right (_,_,v) -> return $ Just v


-- Envoie une requête dans la socket spécifiée.
writePacket :: RawSocket -> Packet -> IO Bool
writePacket sock pkt = sendPkt sock $ L.toStrict $ encode pkt



