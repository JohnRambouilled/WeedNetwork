{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Packets.Layer1 where

import Types.Crypto
import Types.Packets
import Packets.Layer2
import Packets.Communication

import Data.Binary
import qualified Data.ByteString.Lazy as B
import Data.Binary.Put
import Data.Binary.Get
import Data.Functor
import GHC.Generics
import qualified Data.Map as M

neighTimeOut = 15 :: Time
neighRepeatTime = 1 :: Time


data L1 = L1Intro NeighIntro |
          L1Data NeighData |
          L1Pipe PipePacket
    deriving Generic


data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature} 
    deriving Generic
data NeighData = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: L2} 
    deriving Generic


data PipePacket = PipePacket{ pipePacketHeader :: PipeHeader,
                              pipePacketData :: RawData }
                    deriving Show

data PipePacketContent = PPCPipePacket PipePacket |
                         PPCL2 L2 |
                         PPCComPacket ComPacket
        deriving (Show, Generic)


data PipeHeader = PipeHeader {pipeDKeyID :: PipeID,  -- ^PipeID of the pipe used
                              pipeDSig :: Signature,  -- ^ Signature of the packet
                              pipeDPosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                              pipeDDirection :: Bool,  -- ^ Direction of the message (True being the direction followed by the request)
                              pipeDFlags :: [PipeDataFlag] }
              deriving (Generic, Show)

data PipeDataFlag = PipeControlRefresh
                  | PipeClose RawData
              deriving (Show, Generic)

instance Binary L1 
instance Binary NeighData
instance Binary NeighIntro
instance Binary PipePacketContent 

instance Binary PipePacket where put (PipePacket h c) = put h >> putLazyByteString c
                                 get = PipePacket <$> get <*> getRemainingLazyByteString

instance Binary PipeHeader
instance Binary PipeDataFlag

instance SignedClass NeighIntro where scHash (NeighIntro kH pK _) = encode (kH, pK)
                                      scKeyHash = neighIKeyID
                                      scSignature = neighISig
                                      scPushSignature i s = i{neighISig = s}
instance IntroClass NeighIntro where icPubKey = neighIPubKey

instance SignedClass NeighData  where scHash (NeighData  kH _ pay) = encode (kH, pay)
                                      scKeyHash = neighDKeyID
                                      scSignature = neighDSig
                                      scPushSignature d s = d{neighDSig = s}

instance SignedClass PipeHeader where scHash (PipeHeader kH _ n b f) = encode (kH, n, b, f)
                                      scKeyHash = pipeDKeyID
                                      scSignature = pipeDSig
                                      scPushSignature p s = p{pipeDSig = s}

instance SignedClass PipePacket  where scHash (PipePacket h p) = B.append (scHash h)  (encode p)
                                       scKeyHash = scKeyHash . pipePacketHeader
                                       scSignature = pipeDSig . pipePacketHeader
                                       scPushSignature p s = p{pipePacketHeader = scPushSignature (pipePacketHeader p) s}

instance Show NeighIntro where
    show = ("NeighIntro from : " ++ ) . show . neighIKeyID

instance Show NeighData where
    show (NeighData uID _ c) = "NeighData from : " ++ show uID ++ " CONTENT : " ++ show c



