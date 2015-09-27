{-# LANGUAGE DeriveGeneric,MultiParamTypeClasses #-}
module Ressource where

import Class
import Crypto
import Routing

import Data.ByteString hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Control.Lens

import GHC.Generics

type TTL = Int

type DHPubKey = Int

newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord,Generic)

-- TODO
ttlMax = 10
maxDelay = 10


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


type AnswerMap = EventEntryMap RessourceID Answer

instance IDable Answer RessourceID where
        extractID = cResID . ansCert

instance SignedClass Answer where scHash (Answer c _ _ sID r) = encode (c, sID, r)
                                  scKeyHash = ansSourceID
                                  scSignature = cResSig . ansCert
instance IntroClass Answer where icPubKey = cResSourceKey . ansCert 

buildAnswerMap :: Event Research -> Event Answer -> Reactive (Behaviour AnswerMap)
buildAnswerMap rE aE = do  (aMapB,order) <- newBhvTpl M.empty
                           -- listening the researchs
                           listenTrans (filterJust $ snapshot (onResearch order) rE aMapB) id
                           listenTrans (filterJust $ snapshot (flip fireKey) aE aMapB) id
                        
                           pure aMapB
                      
    where onResearch :: Modifier AnswerMap -> Research -> AnswerMap -> Maybe (Reactive ())
          onResearch order r aMap = case resID r `M.lookup` aMap of
                                      Just _ -> Nothing
                                      Nothing -> Just $ do e <- newEventEntry $ (resID r==) . cResID . ansCert
                                                           order $ M.insert (resID r) e



{-
checkCert source time (RessourceCert dhKey pKey sendTime rID sig) = time - sendTime < maxDelay
                                                                    && computeHashFromKey pKey == source
                                                                    && checkSig pKey sig (encode (dhKey,pKey,sendTime,rID))
checkAnswer me time ans = ansTTL ans > 1 && ansTTL ans <= ttlMax &&
                          me `Prelude.notElem` ansRoad ans && checkCert (ansSourceID ans) time (ansCert ans)

-}



instance Binary RessourceID
instance Binary RessourceCert
instance Binary Research
instance Binary Answer

