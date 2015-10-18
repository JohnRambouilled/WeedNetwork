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
type AnswerMapBhv t = ModEvent t AnswerMap

instance IDable Answer RessourceID where
        extractID = cResID . ansCert

instance SignedClass Answer where scHash (Answer c _ _ sID r) = encode (c, sID, r)
                                  scKeyHash = ansSourceID
                                  scSignature = cResSig . ansCert
                                  scPushSignature a s = a{ansCert = (ansCert a){cResSig = s} }
instance IntroClass Answer where icPubKey = cResSourceKey . ansCert 

type RessourceMapTpl = M.Map RessourceID ()
type RelayMapBhv = ModEvent t RessourceMapTpl


data Ressources = Ressources {resAnswerMap :: AnswerMapBhv,
                              resRelayMap :: RelayMapBhv,
                              resRelPackets :: Event RessourcePacket }

buildRessources :: DHPubKey -> KeyPair -> RessourceMapTpl -> Event RessourceID -> Event Research -> Event Answer -> Reactive Ressources
buildRessources dhPK kP locMap rIDE resE ansE = do (relMap, relPE) <- buildRelayMap dhPK kP locMap resE ansE
                                                   ansB <- buildAnswerMap rIDE ansE
                                                   pure $ Ressources ansB relMap relPE


buildRelayMap :: DHPubKey -> KeyPair -> RessourceMapTpl -> Event Research -> Event Answer -> Reactive (RelayMapBhv, Event RessourcePacket)
buildRelayMap dhPK (sK,pK) locRMap resE ansE = do (relMap, relMod) <- newBhvTpl M.empty
                                                  let resP = filterJust $ execute $ snapshot (onResearch relMod) resE relMap
                                                      ansP = filterJust $ snapshot onAnswer ansE relMap
                                                  pure ((relMap, relMod), merge (Left <$> resP) (Right <$> ansP))
        where onResearch :: Modifier RessourceMapTpl -> Research -> RessourceMapTpl -> Reactive (Maybe Research)
              onResearch mod res map = case resID res `M.lookup` map of
                                            Just _ -> pure Nothing
                                            Nothing -> do mod $ M.insert (resID res) ()
                                                          pure . Just $ relayRes res
              onAnswer :: Answer -> RessourceMapTpl -> Maybe Answer
              onAnswer ans map = pure (relayAns ans) <$> extractID ans `M.lookup` map 
              relayAns :: Answer -> Answer
              relayAns = id
              relayRes = id


{- | Event des recherches sortantes, et des Answer entrantes. Construit la map des Event Answer nous concernants.-}
buildAnswerMap :: Event RessourceID -> Event Answer -> Reactive AnswerMapBhv
buildAnswerMap rE aE = do  (aMapB,order) <- newBhvTpl M.empty
                           -- listening the researchs
                           listenTrans (filterJust $ snapshot (onResearch order) rE aMapB) id
                           listenTrans (filterJust $ snapshot (flip fireKey) aE aMapB) id
                        
                           pure (aMapB, order)
                      
    where onResearch :: Modifier AnswerMap -> RessourceID -> AnswerMap -> Maybe (Reactive ())
          onResearch order r aMap = case r `M.lookup` aMap of
                                      Just _ -> Nothing
                                      Nothing -> Just $ do e <- newEventEntry $ (r==) . extractID
                                                           order $ M.insert r e



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

