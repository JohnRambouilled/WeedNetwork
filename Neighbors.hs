{-# LANGUAGE DeriveGeneric #-}
module Neighbors where

import Crypto
import PipePackets
import Class
import Ressource
import Timer
import Routes.Core
import Data.Binary
import Reactive.Banana
import Reactive.Banana.Frameworks
import GHC.Generics
import qualified Data.Map as M

neighTimeOut = 15 :: Time
neighRepeatTime = 1 :: Time

type NeighPacket = Either NeighIntro NeighData

data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature, neighIPayload :: Payload} 
data NeighData  = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: NeighDataContent} 



data NeighDataContent = NeighReq Request | NeighRes RessourcePacket | NeighBrk NeighBreak deriving Generic
type NeighMap = EventCMap KeyHash NeighData
type NeighMapBhv = BehaviorC NeighMap 

data Neighborhood = Neighborhood {nbhNeighMap :: NeighMapBhv,
                                  nbhRequests :: Event Request,
                                  nbhRessources :: Event RessourcePacket,
                                  nbhNeighBreak :: Event NeighBreak,
                                  nbhForceDeco :: Handler UserID}


sendNeighData :: UserID -> KeyPair -> NeighDataContent -> NeighPacket
sendNeighData uID k cnt = Right . sign k $ NeighData uID emptySignature cnt

sendNeighIntro :: UserID -> KeyPair -> Payload -> NeighPacket
sendNeighIntro uID k p = Left . sign k $ NeighIntro uID (fst k) emptySignature p

repeatNeighIntro :: Time -> UserID -> KeyPair -> Payload -> MomentIO (TimeOutEntry, Event NeighPacket)
repeatNeighIntro t uID k p = do (e,h) <- newEvent
                                toe <- liftIO $ newRepeater Nothing t $ h $ sendNeighIntro uID k p
                                pure (toe, e)

buildNeighborhood :: Event NeighPacket -> MomentIO Neighborhood
buildNeighborhood packetE  = let (introE, dataE) = split packetE in
                             do (reqE, reqH) <- newEvent
                                (resE, resH) <- newEvent
                                (brkE, brkH) <- newEvent
                                (decoE, decoH) <- newEvent
                                (nMap, cryptoE) <- buildCryptoMap introE dataE 
                                let (refreshE, newNeighE) = split cryptoE
                                buildTimeOutIDable neighTimeOut newNeighE $ scKeyHash <$> refreshE
                                allEvents <- mergeEvents $ bcChanges nMap
                                reactimate $ closePipe <$> bcLastValue nMap <@> decoE
                                reactimate $ onDataEvent decoH reqH resH brkH <$> allEvents
                                pure $ Neighborhood nMap reqE resE brkE decoH
        where closePipe m k = case k `M.lookup` m of
                                Just ce -> ceClose ce 
                                Nothing -> pure ()


onDataEvent :: Handler KeyHash -> Handler Request -> Handler RessourcePacket -> Handler NeighBreak -> NeighData -> IO ()
onDataEvent _ reqH _ _ (NeighData _ _ (NeighReq r)) = reqH  r
onDataEvent _ _ resH _ (NeighData _ _ (NeighRes r)) = resH  r 
onDataEvent h _ _ nh (NeighData uID _ (NeighBrk nb@(NeighBreak r))) = case r of
                                                                        [] -> pure ()
                                                                        a:[] -> if a == uID then nh nb >> h uID else pure ()
                                                                        a:_ -> if a == uID then nh nb else pure ()
                                                                                                                     



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

