{-# LANGUAGE DeriveGeneric #-}
module Neighbors where

import Crypto
import Routing
import Class
import Ressource
import Timer

import Data.Binary
import qualified Data.Map as M
import Reactive.Banana
import Reactive.Banana.Frameworks
import GHC.Generics

neighTimeOut = 15 :: Time
neighRepeatTime = 1 :: Time

type NeighPacket = Either NeighIntro NeighData

data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature, neighIPayload :: Payload} 
data NeighData  = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: NeighDataContent} |
                  NeighClose {neighDKeyID :: KeyHash, neighDSig :: Signature, neighCPayload :: Payload}

data NeighDataContent = NeighReq Request | NeighRes RessourcePacket deriving Generic
type NeighMap = EventEntryMap KeyHash NeighData
type NeighMapBhv t = ModEvent t NeighMap 

data Neighborhood t = Neighborhood {nbhNeighMap :: NeighMapBhv t,
                                    nbhRequests :: Event t Request,
                                    nbhRessources :: Event t RessourcePacket,
                                    nbhForceDeco :: Handler UserID}


sendNeighData :: UserID -> KeyPair -> NeighDataContent -> NeighPacket
sendNeighData uID k cnt = Right . sign k $ NeighData uID emptySignature cnt

sendNeighIntro :: UserID -> KeyPair -> Payload -> NeighPacket
sendNeighIntro uID k p = Left . sign k $ NeighIntro uID (fst k) emptySignature p

repeatNeighIntro :: Frameworks t => Time -> UserID -> KeyPair -> Payload -> Moment t (TimeOutEntry, Event t NeighPacket)
repeatNeighIntro t uID k p = do (e,h) <- newEvent
                                toe <- liftIO $ newRepeater Nothing t $ h $ sendNeighIntro uID k p
                                pure (toe, e)

buildNeighborhood :: Frameworks t => Event t NeighPacket -> Moment t (Neighborhood t)
buildNeighborhood packetE  = do (introE, dataE) <- splitEither packetE
                                (reqE, reqH) <- newEvent
                                (resE, resH) <- newEvent
                                (decoE, decoH) <- newEvent
                                (nMod, _) <- buildCryptoMap neighTimeOut (pure newEventEntry) introE decoE dataE 
                                allEvents <- mergeEvents $ meChanges nMod
                                reactimate $ onDataEvent decoH reqH resH <$> allEvents
                                pure $ Neighborhood nMod reqE resE decoH


onDataEvent :: Handler KeyHash -> Handler Request -> Handler RessourcePacket -> NeighData -> IO ()
onDataEvent h _ _ (NeighClose kID _ _) = h  kID
onDataEvent _ reqH _ (NeighData _ _ (NeighReq r)) = reqH  r
onDataEvent _ _ resH (NeighData _ _ (NeighRes r)) = resH  r 



instance SignedClass NeighIntro where scHash (NeighIntro kH pK _ pay) = encode (kH, pK, pay)
                                      scKeyHash = neighIKeyID
                                      scSignature = neighISig
                                      scPushSignature i s = i{neighISig = s}
instance IntroClass NeighIntro where icPubKey = neighIPubKey

instance SignedClass NeighData  where scHash (NeighData  kH _ pay) = encode (kH, pay)
                                      scHash (NeighClose kH _ pay) = encode (kH, pay)
                                      scKeyHash = neighDKeyID
                                      scSignature = neighDSig
                                      scPushSignature d s = d{neighDSig = s}


instance Show NeighIntro where
    show = ("NeighIntro from : " ++ ) . show . neighIKeyID

instance Show NeighData where
    show (NeighClose uID _ _) = "NeighClose from : " ++ show uID
    show (NeighData uID _ c) = "NeighData from : " ++ show uID ++ " CONTENT : " ++ show c

instance Show NeighDataContent where show (NeighReq r) = show r
                                     show (NeighRes (Left r)) = show r
                                     show (NeighRes (Right a)) = show a
instance Binary NeighDataContent


