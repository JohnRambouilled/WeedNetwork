{-# LANGUAGE DeriveGeneric #-}
module Ressource where

import Crypto

import Data.ByteString hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Control.Event.Handler

import GHC.Generics

type TTL = Int
type Time = Int

type SourceID = Int
type DHPubKey = Int
type Road = [SourceID]

newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord,Generic)

-- TODO
ttlMax = 10
maxDelay = 10
getTime = pure 10 


data RessourceCert = RessourceCert {cResSourceDHKey :: DHPubKey,
                                    cResSourceKey :: PubKey,
                                    cResTimestamp :: Time,
                                    cResID :: RessourceID,
                                    cResSig :: Signature}
                deriving Generic

type RessourcePacket = Either Research Answer

data Research = Research {resID :: RessourceID,
                          resTTL :: TTL,
                          resRoad :: Road,
                          resCnt :: RawData}
                deriving Generic
data Answer = Answer {ansCert :: RessourceCert,
                      ansTTL :: TTL,
                      ansRoad :: Road,
                      ansSourceID :: SourceID,
                      ansCnt :: RawData}
                deriving Generic
data RessourceEntry = RessourceEntry {researchHandler :: Handler Research,
                                      answerHandler   :: Handler Answer}


data RessourceOrder = RessourceAdd RessourceID RessourceEntry
                    | RessourceDelete RessourceID

type RessourceMap = M.Map RessourceID RessourceEntry

{-| Called on each datapacket |-}
--buildRessource :: Handler Packet -> Handler Payload


buildRessource :: (Frameworks t) => KeyHash -> Handler RessourcePacket -> EventSource RessourceOrder ->  Event t RessourcePacket -> Moment t ()
buildRessource me sendH (ordSource, fireOrder) ePkt = do ordE <- fromAddHandler ordSource
                                                         let (eResearch,eAnswer) = split ePkt
                                                             resM = accumB M.empty (onRessourceOrder <$> ordE)
                                                         reactimate $ onResearch me fireOrder (sendH . Right) <$> resM <@> eResearch
                                                         reactimate $ onAnswer me <$> resM <@> eAnswer


checkCert source time (RessourceCert dhKey pKey sendTime rID sig) = time - sendTime < maxDelay
                                                                    && computeHashFromKey pKey == source
                                                                    && checkSig pKey sig (encode (dhKey,pKey,sendTime,rID))
checkAnswer me time ans = ansTTL ans > 1 && ansTTL ans <= ttlMax &&
                          me `Prelude.notElem` ansRoad ans && checkCert (ansSourceID ans) time (ansCert ans)


onResearch :: KeyHash -> Handler RessourceOrder -> Handler Answer -> RessourceMap -> Research -> IO ()
onResearch me ordH outH resM r@(Research rID rTTL rRoad rCnt) = when (checkResearch r) $
                                                                     case rID `M.lookup` resM of 
                                                                           Just rE -> researchHandler rE r 
                                                                           Nothing -> do ordH $ RessourceAdd rID (RessourceEntry (pure $ pure ()) relayAns)  
    where relayAns :: Handler Answer
          relayAns ans@(Answer crt ttl road sID pay) = when (ttl > 1) $ 
                                                            outH $ ans{ ansTTL=ttl-1, 
                                                                        ansRoad = me:road}
          checkResearch res = resTTL res > 0 && resTTL res <= ttlMax &&
                              me `Prelude.notElem` resRoad res


onAnswer :: KeyHash -> RessourceMap -> Handler Answer
onAnswer me resM ans = do time <- getTime
                          when (checkAnswer me time ans) $
                            case (cResID $ ansCert ans) `M.lookup` resM of
                                Just rE -> answerHandler rE ans
                                Nothing -> pure ()


onRessourceOrder :: RessourceOrder -> RessourceMap -> RessourceMap
onRessourceOrder (RessourceAdd rID rE) resM = M.insert rID rE resM 
onRessourceOrder (RessourceDelete rID) resM = M.delete rID resM 




instance Binary RessourceID
instance Binary RessourceCert
instance Binary Research
instance Binary Answer

