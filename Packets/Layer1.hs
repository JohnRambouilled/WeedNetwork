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
                             _neighDContent  :: L2,
                             _neighDSig      :: Signature}
    deriving Generic

data PipePacket = PipePacket{ _pipeSource       :: KeyHash,  -- Not signed, updated at each transmission
                              _pipeDestinary    :: KeyHash,  -- Not signed, updated at each transmission
                              _pipePID :: PipeID,
                              _pipeFlags :: [PipePacketFlag],
                              _pipeSig :: Signature,
                              _pipePacketData    :: RawData}
                    deriving Show

data PipePacketContent = PPCPipePacket PipePacket |
                         PPCL2 L2 |
                         PPCComPacket ComPacket
        deriving (Show, Generic)

data PipePacketFlag = PipeControlRefresh
                    | PipeControlClose 
              deriving (Eq, Show, Generic)

makeLenses ''NeighIntro
makeLenses ''PipePacket
makeLenses ''NeighData
makeLenses ''L1

instance Binary NeighDestinary
instance Binary L1
instance Binary NeighData
instance Binary NeighIntro
instance Binary PipePacketContent

instance Binary PipePacket where put (PipePacket src dst pID f s c) = put src >> put dst >> put pID >> put f >> put s >> putLazyByteString c
                                 get = PipePacket <$> get <*> get <*> get <*> get <*>  get <*>  getRemainingLazyByteString

instance Binary PipePacketFlag

instance SignedClass NeighIntro where scHash (NeighIntro kH pK _) = encode (kH, pK)
                                      scSignature = view neighISig
                                      scPushSignature i s = set neighISig s i
instance IntroClass NeighIntro where icPubKey = _neighIPubKey

instance SignedClass NeighData  where scHash (NeighData src dst pay _) = encode (src, dst, pay)
                                      scSignature = view neighDSig
                                      scPushSignature d s = set neighDSig s d

instance SignedClass PipePacket  where scHash (PipePacket src dst pid f p _) = encode (src, dst, pid, f, p)
                                       scSignature = view pipeSig 
                                       scPushSignature p s = set pipeSig s p

instance Show NeighIntro where
    show = ("NeighIntro from : " ++ ) . show . _neighISource

instance Show NeighData where
    show (NeighData uID _ _ c) = "NeighData from : " ++ show uID ++ " CONTENT : " ++ show c



