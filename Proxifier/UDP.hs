{-# LANGUAGE ForeignFunctionInterface #-}

module Proxifier.UDP where

import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import Data.ByteString.Char8 hiding (putStrLn)
import Network.Socket

foreign import ccall "Proxifier/udp.c udp_buf"
    c_udp_buf :: Ptr () -> IO (Ptr CChar)
foreign import ccall "Proxifier/udp.c udp_size"
    c_udp_size :: Ptr () -> IO CLong
foreign import ccall "Proxifier/udp.c udp_src"
    c_udp_src :: Ptr () -> IO CString

foreign import ccall "Proxifier/udp.c udp_nextPacket"
    c_udp_nextPacket :: CInt -> CLong -> IO (Ptr ())

foreign import ccall "Proxifier/udp.c free_udp_packet"
    c_udp_free :: Ptr () -> IO ()

foreign import ccall "Proxifier/udp.c set_block"
    c_set_block :: CInt -> IO ()





nextUDPPacket :: Socket -> Int -> IO (String,ByteString)
nextUDPPacket sock maxSiz = do pkt <- c_udp_nextPacket (fdSocket sock) (fromIntegral maxSiz)
                               (buf,siz,src) <- (,,) <$> c_udp_buf pkt <*> c_udp_size pkt <*> c_udp_src pkt
                               --putStrLn $ "[HASKELL] received " ++ show siz ++ "bytes." 
                               srcFile <- peekCString src
                               --putStrLn $ "[HASKELL] file = " ++ show srcFile
                               udpRaw <- peekCStringLen (buf,fromIntegral siz)
                               c_udp_free pkt
                               return (srcFile,pack udpRaw)
                               
