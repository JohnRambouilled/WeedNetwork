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
type NeighMap = ChannelMap KeyHash NeighData
type NeighMapBhv = BehaviorC NeighMap 

-- | Object created when a new neighbor appears. Contains the ID and the Key of the neighbor, and then channel containing it's future messages
data Neighbor = Neighbor { neighID :: KeyHash, neighPubKey :: PubKey, neighMessages :: Channel NeighDataContent}

data Neighborhood = Neighborhood {nbhNeighMap :: NeighMapBhv,
                                  newNeighborE :: Event Neighbor,
                                  nbhRequests :: Event Request,
                                  nbhRessources :: Event RessourcePacket,
                                  nbhNeighBreak :: Event NeighBreak}


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
                             do (nMap, cryptoE) <- buildCryptoMap introE dataE 
                                let (refreshE, newE) = split cryptoE
                                newNeighE <- execute $ makeNewNeighbor <$> newE
                                buildTimeOutIDable neighTimeOut newE $ scKeyHash <$> refreshE
                                (reqE, resE, brkE) <- splitNeighPacket . fmap neighDContent <$> mergeEvents (bcChanges nMap)
                                pure $ Neighborhood nMap newNeighE reqE resE brkE 
        where makeNewNeighbor :: (NeighIntro, Channel NeighData) -> MomentIO Neighbor
              makeNewNeighbor (intro, chan) = do reactimate $ onBrk <$> chanEvent chan
                                                 return $ Neighbor (neighIKeyID intro) (neighIPubKey intro) $ neighDContent <$> chan
                    where onBrk :: NeighData -> IO ()
                          onBrk (NeighData uID _ (NeighBrk (NeighBreak r))) = case r of
                                        [] -> chanLogH chan $ "ERROR : Invalid NeighBreak (road empty)"
                                        a:[] -> if a == uID then chanCloseH chan >> (chanLogH chan) "Closing channel after receiving NeighBreak from neighbor"
                                                          else chanLogH chan $ "ERROR : Invalid NeighBreak (sender ID and head of the road doesn't match"
                                        a:_ -> if a == uID then chanLogH chan $ "Relayed NeighBreak received"
                                                         else chanLogH chan $ "ERROR : Invalid NeighBreak (sender ID and head of the road doesn't match"
                          onBrk _ = pure ()





splitNeighPacket :: Event NeighDataContent -> (Event Request, Event RessourcePacket, Event NeighBreak)
splitNeighPacket e = let (e1, brkE) = split $ onDataEvent <$> e
                         (reqE, resE) = split e1
                     in (reqE, resE, brkE)
    where onDataEvent :: NeighDataContent -> Either (Either Request RessourcePacket) NeighBreak
          onDataEvent (NeighReq r) = Left $ Left r
          onDataEvent (NeighRes r) = Left $ Right r 
          onDataEvent (NeighBrk nb) = Right nb
                                                                                                                        



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

