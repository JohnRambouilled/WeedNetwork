{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Packets.Layer1 where

import           Control.Lens
import           Packets.Communication
import           Packets.Layer2
import           Types.Crypto
import           Types.Packets

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy  as B
import           Data.Functor
import qualified Data.Map              as M
import           GHC.Generics

neighTimeOut = 15 :: Time
neighRepeatTime = 1 :: Time


data NeighDestinary = Broadcast | UserDest KeyHash
    deriving Generic


data L1 = L1Intro NeighIntro | L1Data NeighData | L1Pipe PipePacket
    deriving Generic



data NeighIntro = NeighIntro {_neighISource :: KeyHash,
                              _neighIPubKey :: PubKey,
                              _neighISig    :: Signature}
    deriving Generic

data NeighData = NeighData  {_neighDSource   :: KeyHash,
                             _neighDestinary :: NeighDestinary,
                             _neighDSig      :: Signature,
                             _neighDContent  :: L2}
    deriving Generic

data PipePacket = PipePacket{ _pipeSource       :: KeyHash,  -- Not signed, updated at each transmission
                              _pipeDestinary    :: KeyHash,  -- Not signed, updated at each transmission
                              _pipePacketHeader :: PipeHeader,
                             _pipePacketData    :: RawData }
                    deriving Show


data PipePacketContent = PPCPipePacket PipePacket |
                         PPCL2 L2 |
                         PPCComPacket ComPacket
        deriving (Show, Generic)


data PipeHeader = PipeHeader {_pipeDID :: PipeID,    -- ^PipeID (KeyHash of the road)
                              _pipeDSig   :: Signature,  -- ^ Signature of the packet
                              _pipeDFlags :: [PipeDataFlag] }
              deriving (Generic, Show)

data PipeDataFlag = PipeControlRefresh
                  | PipeClose RawData
              deriving (Show, Generic)
makeLenses ''PipeHeader
makeLenses ''PipePacket
makeLenses ''NeighData
makeLenses ''L1
instance Binary NeighDestinary
instance Binary L1
instance Binary NeighData
instance Binary NeighIntro
instance Binary PipePacketContent

instance Binary PipePacket where put (PipePacket src dst h c) = put src >> put dst >> put h >> putLazyByteString c
                                 get = PipePacket <$> get <*> get <*> get <*> getRemainingLazyByteString

instance Binary PipeHeader
instance Binary PipeDataFlag

instance SignedClass NeighIntro where scHash (NeighIntro kH pK _) = encode (kH, pK)
                                      scSignature = _neighISig
                                      scPushSignature i s = i{_neighISig = s}
instance IntroClass NeighIntro where icPubKey = _neighIPubKey

instance SignedClass NeighData  where scHash (NeighData src dst _ pay) = encode (src,dst, pay)
                                      scSignature = _neighDSig
                                      scPushSignature d s = d{_neighDSig = s}

instance SignedClass PipeHeader where scHash (PipeHeader kH _ flags) = encode (kH, flags)
                                      scSignature = _pipeDSig
                                      scPushSignature p s = set pipeDSig s p
--                                      scPushSignature p s = p{_pipeDSig = s}

instance SignedClass PipePacket  where scHash (PipePacket src dst h p) = B.concat [encode $ (src,dst), (scHash h), (encode p)]
                                       scSignature = _pipeDSig . _pipePacketHeader
                                       scPushSignature p s = over pipePacketHeader (flip scPushSignature s) p

instance Show NeighIntro where
    show = ("NeighIntro from : " ++ ) . show . _neighISource

instance Show NeighData where
    show (NeighData uID _ _ c) = "NeighData from : " ++ show uID ++ " CONTENT : " ++ show c



