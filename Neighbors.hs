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

type NeighPacket = Either NeighIntro NeighData

data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature, neighIPayload :: Payload} 
data NeighData  = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: NeighDataContent} |
                  NeighClose {neighDKeyID :: KeyHash, neighDSig :: Signature, neighCPayload :: Payload}

data NeighDataContent = NeighReq Request | NeighRes Research | NeighAns Answer deriving Generic
type NeighMap = TimeMap KeyHash (EventEntry NeighData)
type NeighMapBhv t = ModEvent t NeighMap 

data Neighborhood t = Neighborhood {nbhNeighMap :: NeighMapBhv t,
                                    nbhRequests :: Event t Request,
                                    nbhResearchs :: Event t Research,
                                    nbhAnswers :: Event t Answer}

neighTimeOut = 10 :: Time
neighRepeatTime = 15 :: Time
repeatNeighIntro :: Frameworks t => Time -> UserID -> KeyPair -> Payload -> Moment t (TimeOutEntry, Event t NeighPacket)
repeatNeighIntro t uID k p = do (e,h) <- newEvent
                                toe <- liftIO $ newRepeater Nothing t $ h neighP
                                pure (toe, e)
        where neighP = Left . sign k $ NeighIntro uID (fst k) emptySignature p

buildNeighborhood :: Frameworks t => Event t NeighPacket -> Moment t (Neighborhood t)
buildNeighborhood packetE  = do (introE, dataE) <- splitEither packetE
                                (nMod, _) <- buildCryptoMapTO neighTimeOut introE dataE
                                (reqE, reqH) <- newEvent
                                (resE, resH) <- newEvent
                                (ansE, ansH) <- newEvent
                                allEvents <- mergeEvents $ meChanges nMod
                                reactimate $ onDataEvent (meModifier nMod . M.delete) reqH resH ansH <$> allEvents
                                pure $ Neighborhood nMod reqE resE ansE



onDataEvent :: Handler KeyHash -> Handler Request -> Handler Research -> Handler Answer -> NeighData -> IO ()
onDataEvent h _ _ _ (NeighClose kID _ _) = h  kID
onDataEvent _ reqH _ _ (NeighData _ _ (NeighReq r)) = reqH  r
onDataEvent _ _ resH _ (NeighData _ _ (NeighRes r)) = resH  r 
onDataEvent _ _ _ ansH (NeighData _ _ (NeighAns a)) = ansH  a



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
                                     show (NeighRes r) = show r
                                     show (NeighAns a) = show a
instance Binary NeighDataContent


