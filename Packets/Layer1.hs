{-# LANGUAGE DeriveGeneric #-}
module Packets.Layer1 where

import Types.Crypto
import Types.Packets
import Packets.Ressource

import Control.Monad.Free
import Data.Binary
import GHC.Generics
import qualified Data.Map as M

neighTimeOut = 15 :: Time
neighRepeatTime = 1 :: Time


data L1 = L1Intro NeighIntro |
          L1Data NeighData |
          L1Pipe (Free PipePacket (PipePacket L2))
    deriving Generic


data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature} 
data NeighData = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: L2} 

type PipePacket = Free PipeData (PipeData L2)
data PipeData a = PipeData {pipeDKeyID :: PipeID,  -- ^PipeID of the pipe used
                            pipeDSig :: Signature,  -- ^ Signature of the packet
                            pipeDPosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                            pipeDDirection :: Bool,  -- ^ Direction of the message (True being the direction followed by the request)
                            pipeDPayload :: a}  -- ^ Content of the message (this one should be useful)
              deriving Generic

instance Binary L1

instance (Binary a) => Binary (PipeData a)
instance Binary PipePacket


instance SignedClass NeighIntro where scHash (NeighIntro kH pK _) = encode (kH, pK)
                                      scKeyHash = neighIKeyID
                                      scSignature = neighISig
                                      scPushSignature i s = i{neighISig = s}
instance IntroClass NeighIntro where icPubKey = neighIPubKey

instance SignedClass NeighData  where scHash (NeighData  kH _ pay) = encode (kH, pay)
                                      scKeyHash = neighDKeyID
                                      scSignature = neighDSig
                                      scPushSignature d s = d{neighDSig = s}


instance Show NeighIntro where
    show = ("NeighIntro from : " ++ ) . show . neighIKeyID

instance Show NeighData where
    show (NeighData uID _ c) = "NeighData from : " ++ show uID ++ " CONTENT : " ++ show c

instance Show NeighDataContent where show (NeighReq r) = show r
                                     show (NeighRes (Left r)) = show r
                                     show (NeighRes (Right a)) = show a
                                     show (NeighBrk brk) = show brk
instance Binary NeighDataContent
instance Binary NeighBreak



data PipeControlMessage = PipeControlRefresh
                        | PipeClose RawData
              deriving Generic


instance Binary PipeData

instance SignedClass PipeData where scHash (PipeData kH _ n b m) = encode (kH, n, b, m)
                                    scKeyHash = pipeDKeyID
                                    scSignature = pipeDSig
                                    scPushSignature p s = p{pipeDSig = s}

