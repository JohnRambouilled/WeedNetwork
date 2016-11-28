{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Packets.Layer1 where

import Types.Crypto
import Types.Packets
import Packets.Layer2

import Control.Monad.Free
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

type PipeFree = Free PipeData (PipeData L2)

data PipePacket = PipePacket{ pipePacketDepth :: Int,
                              pipePacketData :: PipeData RawData }

data PipeData a = PipeData {pipeHeader :: PipeHeader,
                                pipeContent :: a}
instance Functor PipeData where fmap f (PipeData h x) = PipeData h (f x)

data PipeHeader = PipeHeader {pipeDKeyID :: PipeID,  -- ^PipeID of the pipe used
                              pipeDSig :: Signature,  -- ^ Signature of the packet
                              pipeDPosition :: Number, -- ^ Position on the pipe, changed during routing (NOT SIGNED)
                              pipeDDirection :: Bool,  -- ^ Direction of the message (True being the direction followed by the request)
                              pipeDFlags :: [PipeDataFlag] }
              deriving Generic

data PipeDataFlag = PipeControlRefresh
                  | PipeClose RawData
              deriving Generic

instance Binary L1 
instance Binary NeighData
instance Binary NeighIntro

pipePacketToPipeFree :: PipePacket -> PipeFree
pipePacketToPipeFree (PipePacket n (PipeData h d)) = unwrap (PipeData h ()) (decode d :: Free PipeData L2)
                    where --unwrap :: f () -> Free f a -> Free f (f a)    Est-ce possible? Si oui pourquoi  pas dans Hackage? j'ai pas l'impression, ou alors c'est que je suis trop con...
                          unwrap :: PipeData () -> Free PipeData L2 -> PipeFree
                          unwrap f (Pure a) = Pure (f $> a)
                          unwrap f (Free f') = Free $ f $> (unwrap (f' $> ()) $ pipeContent f')
                                           
--                          unwrap f (Free f') = Free $ (unwrap (() <$ f') a) <$ f

instance Binary PipePacket where put (PipePacket n (PipeData h c)) = put n >> put h >> putLazyByteString c
                                 get = PipePacket <$> get <*> (PipeData <$> get <*> getRemainingLazyByteString)


instance Binary a => Binary (Free PipeData a) where put = put' (pure ()) (0 :: Int)
                                                            where put' :: Binary a => Put -> Int -> Free PipeData a -> Put
                                                                  put' s n (Pure a) = put n >> s >> put a
                                                                  put' s n (Free (PipeData h p)) = put' (s >> put h) (n+1) p
                                                    get = do n <- get :: Get Int
                                                             get' n
                                                            where get' :: Binary a => Int -> Get (Free PipeData a)
                                                                  get' 0 = Pure <$> get
                                                                  get' n = Free <$> (PipeData <$> get <*> get' (n-1) )

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


instance Binary a => SignedClass (PipeData a) where scHash (PipeData h p) = B.append (scHash h)  (encode p)
                                                    scKeyHash = scKeyHash . pipeHeader
                                                    scSignature = pipeDSig . pipeHeader
                                                    scPushSignature p s = p{pipeHeader = scPushSignature (pipeHeader p) s}

instance Show NeighIntro where
    show = ("NeighIntro from : " ++ ) . show . neighIKeyID

instance Show NeighData where
    show (NeighData uID _ c) = "NeighData from : " ++ show uID ++ " CONTENT : " ++ show c



