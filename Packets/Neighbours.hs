{-# LANGUAGE DeriveGeneric #-}
module Packets.Neighbours where

import Types.Crypto
import Packets.PipePackets
import Packets.Ressource
import Data.Binary
import GHC.Generics
import qualified Data.Map as M

neighTimeOut = 15 :: Time
neighRepeatTime = 1 :: Time

type NeighPacket = Either NeighIntro NeighData

data NeighBreak = NeighBreak {neighBOrigin :: UserID, neighBPipes :: [PipeID]} deriving (Show, Eq, Generic)


data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature, neighIPayload :: Payload} 
data NeighData  = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: NeighDataContent} 



data NeighDataContent = NeighReq Request | NeighRes RessourcePacket | NeighBrk NeighBreak deriving Generic



instance SignedClass NeighIntro where scHash (NeighIntro kH pK _ pay) = encode (kH, pK, pay)
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

