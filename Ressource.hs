{-# LANGUAGE DeriveGeneric,MultiParamTypeClasses #-}
module Ressource where

import Class
import Crypto
import Routing
import Timer

import Data.ByteString hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Control.Lens
import Reactive.Banana
import Reactive.Banana.Frameworks

import GHC.Generics

type TTL = Int

newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord,Generic, Show)

-- TODO
ttlMax = 10
maxDelay = 10



type AnswerMap = EventEntryMap RessourceID Answer
type AnswerMapBhv t = ModEvent t AnswerMap

type RelayMap = TimeMap RessourceID ()
type RelayMapBhv t = ModEvent t RelayMap 

type LocalRessourceMap = M.Map RessourceID (Int, Time, RawData) --Nombre de répetition, et temps d'attente, contenue des réponses 
genLocalResMap :: [RessourceID] -> LocalRessourceMap
genLocalResMap rIDL = M.fromList $ f <$> rIDL
    where f i = (i, (5, 5, encode i))



data Ressources t = Ressources {resAnswerMap :: AnswerMapBhv t,
                                resRelayMap :: RelayMapBhv t,
                                resRelPackets :: Event t RessourcePacket }

genResearch :: RessourceID -> RessourcePacket
genResearch rID = Left $ Research rID ttlMax [] emptyPayload


buildRessources :: Frameworks t => DHPubKey -> UserID -> KeyPair -> LocalRessourceMap -> Event t Research -> Event t Answer -> Event t RessourceID -> Moment t (Ressources t)
buildRessources dhPK uID kP locMap resE ansE rIDE = do (relMap, relPE) <- buildRelayMap dhPK uID kP locMap resE ansE
                                                       ansB <- buildAnswerMap rIDE ansE
                                                       pure $ Ressources ansB relMap (union relPE $ genResearch <$> rIDE)

relayTimeOut = 10 :: Time
buildRelayMap :: Frameworks t => DHPubKey -> UserID -> KeyPair -> LocalRessourceMap -> Event t Research -> Event t Answer -> Moment t (RelayMapBhv t, Event t RessourcePacket)
buildRelayMap dhPK uID (pK,sK) locRMap resE ansE = do relModE <- newModEvent M.empty
                                                      (resFE, locAnsE) <- answerResearch resE 
                                                      let resP = filterJust $  applyMod onResearch relModE resFE 
                                                          ansP = filterJust $ applyMod (pure onAnswer) relModE ansE 
                                                      insertTOReactimate_ relayTimeOut relModE $ (\r -> (resID r, ())) <$> resP
                                                      pure (relModE, unions [Left <$> resP, Right <$> ansP, Right <$> locAnsE ])
        where answerResearch :: Frameworks t => Event t Research -> Moment t (Event t Research, Event t Answer)
              answerResearch e = do (ansE, ansH) <- newEvent
                                    let e' = f ansH <$> e
                                    reactimate $ fst  <$> e'
                                    pure (filterJust $ snd  <$> e', ansE)
                        where f h res = case resID res `M.lookup` locRMap of
                                            Nothing -> (pure (), Just res)
                                            Just (n,t,c) -> (void . newRepeater (Just n) t $ sendAnswer h c (resID res), Nothing)

              onResearch :: Modifier RelayMap -> RelayMap -> Research -> Maybe Research
              onResearch mod map res = case resID res `M.lookup` map of
                                            Just _ -> Nothing
                                            Nothing ->  Just $ relayRes res
              onAnswer :: RelayMap -> Answer -> Maybe Answer
              onAnswer map ans = pure (relayAns ans) <$> extractID ans `M.lookup` map 
              relayAns :: Answer -> Answer
              relayAns a = a{ansTTL = ansTTL a - 1, ansRoad = uID : ansRoad a}
              relayRes :: Research -> Research
              relayRes res = res{resTTL = resTTL res - 1}
              sendAnswer :: Handler Answer -> RawData -> RessourceID -> IO ()
              sendAnswer h c rID = do t <- getTime
                                      let cert = RessourceCert dhPK pK t rID emptySignature
                                      h $ Answer cert ttlMax [uID] uID c




{- | Event des recherches sortantes, et des Answer entrantes. Construit la map des Event Answer nous concernants.-}
buildAnswerMap :: Frameworks t => Event t RessourceID -> Event t Answer -> Moment t (AnswerMapBhv t)
buildAnswerMap rE aE = do  aModE <- newModEvent M.empty
                           -- listening the researchs
                           reactimate $ applyMod onResearch aModE rE
                           reactimate . filterJust $ apply (fireKey <$> meLastValue aModE) aE
                           pure aModE
    where onResearch :: Modifier AnswerMap -> AnswerMap -> RessourceID -> IO ()
          onResearch order aMap r = case r `M.lookup` aMap of
                                      Just _ -> pure ()
                                      Nothing ->  do e <- newEventEntry $ (r==) . extractID
                                                     order $ M.insert r e


answerToNewRoad :: UserID -> Answer -> NewRoad
answerToNewRoad uID = NewRoad <$> (uID :) . ansRoad <*> cResSourceDHKey . ansCert <*> ansSourceID <*> pure (encode "wooobdidoo")

{-
checkCert source time (RessourceCert dhKey pKey sendTime rID sig) = time - sendTime < maxDelay
                                                                    && computeHashFromKey pKey == source
                                                                    && checkSig pKey sig (encode (dhKey,pKey,sendTime,rID))
checkAnswer me time ans = ansTTL ans > 1 && ansTTL ans <= ttlMax &&
                          me `Prelude.notElem` ansRoad ans && checkCert (ansSourceID ans) time (ansCert ans)

-}
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
                                    cResID :: RessourceID,
                                    cResSig :: Signature}
                deriving Generic


instance IDable Answer RessourceID where
        extractID = cResID . ansCert

instance SignedClass Answer where scHash (Answer c _ _ sID r) = encode (c, sID, r)
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


