{-# LANGUAGE DeriveGeneric,MultiParamTypeClasses #-}
module Packets.Ressource where

import Types.Crypto
import Packets.PipePackets


import Data.Binary
import Control.Monad
import qualified Data.Map as M

import GHC.Generics

type TTL = Int

newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord,Generic, Show)

-- TODO
ttlMax = 10
ansMaxDelay = 10

relayTimeOut = 10 :: Time


checkAnswer :: UserID -> Time -> Answer -> Either String Answer
checkAnswer me = checkAns me
    where checkAns me t ans@(Answer (RessourceCert _ pK ts val _ _) ttl r sID cnt)
            | ttl <= 1 || ttl > ttlMax      = Left "Incorrect TTL"
            | me `Prelude.elem` r          = Left "Already in the road" 
            | t - ts > val                 = Left "Answer obsolete"
            | computeHashFromKey pK /= sID  = Left "SourceID does not match PublicKey"
            | checkSig pK ans              = Right (relayAnswer ans)
            | otherwise                    = Left "Incorrect signature"
          relayAnswer :: Answer -> Answer
          relayAnswer a = a{ansTTL = ansTTL a - 1, ansRoad = me : ansRoad a}

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


data RessourceCert = RessourceCert {cResSourceDHKey :: DHPubKey,
                                    cResSourceKey :: PubKey,
                                    cResTimestamp :: Time,
                                    cResValidity :: Time,
                                    cResID :: RessourceID,
                                    cResSig :: Signature}
                deriving Generic



instance SignedClass Answer where scHash (Answer (RessourceCert d k t t' rid _) _ _ sID r) = encode (d, k, t, t', rid, sID, r)
                                  scKeyHash = ansSourceID
                                  scSignature = cResSig . ansCert
                                  scPushSignature a s = a{ansCert = (ansCert a){cResSig = s} }
instance IntroClass Answer where icPubKey = cResSourceKey . ansCert 



instance Show Research where show (Research rID ttl _ _) = "Research for : " ++ show rID ++ " ttl : " ++ show ttl
instance Show Answer where show (Answer c ttl r sID _ ) = "Answer for : " ++ show (cResID c) ++ " from : " ++ show sID ++ " ttl : " ++ show ttl ++ " ROAD : " ++ show r
instance Binary RessourceID
instance Binary RessourceCert
instance Binary Research
instance Binary Answer


