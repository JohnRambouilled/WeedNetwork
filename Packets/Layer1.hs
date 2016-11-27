{-# LANGUAGE DeriveGeneric #-}
module Packets.Layer1 where

import Types.Crypto
import Types.Packets
import Packets.Layer2

import Control.Monad.Free
import Data.Binary
import GHC.Generics
import qualified Data.Map as M

neighTimeOut = 15 :: Time
neighRepeatTime = 1 :: Time


data L1 = L1Intro NeighIntro |
          L1Data NeighData |
          L1Pipe PipePacket
    deriving Generic


data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature} 
data NeighData = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: L2} 

type PipePacket = Free PipeData (PipeData L2)
data PipeData a = PipeData {pipeDKeyID :: PipeID,  -- ^PipeID of the pipe used
                            pipeDSig :: Signature,  -- ^ Signature of the packet
                            pipeDPosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                            pipeDDirection :: Bool,  -- ^ Direction of the message (True being the direction followed by the request)
                            pipeDFlags :: [PipeDataFlag],
                            pipeDPayload :: a}  -- ^ Content of the message (this one should be useful)
              deriving Generic

data PipeDataFlag = PipeControlRefresh
                  | PipeClose RawData
              deriving Generic

instance Binary L1

instance Binary a => Binary (Free PipeData a) where put f = iter putPD (put <$> f)
                                                        where putPD :: PipeData Put -> Put
                                                              putPD = put . (runPut <$>)

instance (Binary a) => Binary (PipeData a)

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

instance Binary a => SignedClass (PipeData a) where scHash (PipeData kH _ n b f m) = encode (kH, n, b, f, m)
                                                    scKeyHash = pipeDKeyID
                                                    scSignature = pipeDSig
                                                    scPushSignature p s = p{pipeDSig = s}

instance Show NeighIntro where
    show = ("NeighIntro from : " ++ ) . show . neighIKeyID

instance Show NeighData where
    show (NeighData uID _ c) = "NeighData from : " ++ show uID ++ " CONTENT : " ++ show c



