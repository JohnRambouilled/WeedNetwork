{-# LANGUAGE DeriveGeneric,MultiParamTypeClasses #-}
module Ressource where

import Class
import Crypto
--import Routing
import PipePackets
import Timer

import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana
import Reactive.Banana.Frameworks

import GHC.Generics

type TTL = Int

newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord,Generic, Show)

-- TODO
ttlMax = 10
maxDelay = 10

relayTimeOut = 10 :: Time

defaultRelayPolitic = True
defaultStorePolitic = True
defaultListenPolitic = False

type LocalAnswer = (Time, RawData) --Durée de validité, et contenu

type ResPolMap = M.Map RessourceID Bool
type ResPolMapBhv = BehaviorMod ResPolMap

type AnswerMap = TimeMap RessourceID Answer --Map des Answers disponnibles
type AnswerMapBhv = BehaviorMod AnswerMap

type LocalAnswerMap = M.Map RessourceID LocalAnswer
type LocalAnswerMapBhv = BehaviorMod LocalAnswerMap

type RelayMap = TimeMap RessourceID ()
type RelayMapBhv = BehaviorMod RelayMap 

--type LocalRessourceMap = M.Map RessourceID (Int, Time, RawData) --Nombre de répetition, et temps d'attente, contenue des réponses 
--genLocalResMap :: [RessourceID] -> LocalRessourceMap
--genLocalResMap rIDL = M.fromList $ f <$> rIDL
--    where f i = (i, (5, 5, encode i))



data Ressources = Ressources {resAnswerMap :: AnswerMapBhv,
                              resLocalAnswerMap :: LocalAnswerMapBhv,
                              resListenMap :: BehaviorC (EventMap RessourceID Answer),
                              resRelayMap :: RelayMapBhv,
                              resRelayPolitic :: ResPolMapBhv,
                              resStorePolitic :: ResPolMapBhv,
                              resListenHandler :: Handler (RessourceID, Bool),
                              resRelPackets :: Event RessourcePacket }

genResearch :: RessourceID -> RessourcePacket
genResearch rID = Left $ Research rID ttlMax [] emptyPayload

offerRessource :: Ressources -> (Time, RawData, RessourceID) -> IO ()
offerRessource res (t,d,rID) = bmModifier (resLocalAnswerMap res) $ M.insert rID (t,d)

buildRessources :: DHPubKey -> UserID -> KeyPair -> Event RessourcePacket -> MomentIO Ressources
buildRessources dhPK uID kP packetE = let (resE, ansE) = split packetE in
                                      do [relPol, stoPol] <- forM [1..2] $ pure $ newBehaviorMod M.empty
                                         ansB <- newBehaviorMod M.empty
                                         locAnsB <- newBehaviorMod M.empty
                                         relB <- newBehaviorMod M.empty
                                         (listenE, listenH) <- newEvent
                                         (packetOE, packetOH) <- newEvent
                                         let onResB = onResearch packetOH (bmModifier relB) <$> bmLastValue relB <*> bmLastValue ansB <*> bmLastValue locAnsB <*> bmLastValue relPol
                                             onAnsB = onAnswer packetOH (bmModifier ansB) <$> bmLastValue ansB <*> bmLastValue relB <*> bmLastValue stoPol
                                         reactimate $ apply onResB resE
                                         reactimate $ apply onAnsB ansE
                                         listenB <- buildListenMap listenE ansE
                                         pure $ Ressources ansB locAnsB listenB relB relPol stoPol listenH packetOE 

           {- Si la ressource est présente dans une des AnswerMap, on envoi la réponse. Sinon, on relay si elle est absente de la RelayMap et insert une entrée (time-out),
            - et on ignore si elle est présente dans la RelayMap -}
    where onResearch :: Handler RessourcePacket -> Modifier RelayMap -> RelayMap -> AnswerMap -> LocalAnswerMap -> ResPolMap -> Research -> IO () 
          onResearch packetH modRelM relM ansM locAnsM relPol res = when b $ case rID `M.lookup` locAnsM of
                        Just locA -> sendAnswer dhPK (fst kP) uID packetH locA rID
                        Nothing -> case rID `lookupTO` ansM of
                                    Just a -> packetH $ Right a
                                    Nothing -> case rID `lookupTO` relM of
                                                Just _ -> pure ()
                                                Nothing -> do e <- newTimeOutEntry relayTimeOut $ modRelM $ M.delete rID
                                                              modRelM $ M.insert rID (e,())
                                                              packetH $ Left (relayResearch res)
                    where rID = resID res
                          b = maybe defaultRelayPolitic id $ rID `M.lookup` relPol
          {- Si la AnswerMap contient déja une meilleur Answer, on ignore.
           - Sinon, on relai si la ressource est présente dans la relayMap,
           - et on insère dans la AnswerMap si la politique le permet-}
          onAnswer :: Handler RessourcePacket -> Modifier AnswerMap -> AnswerMap -> RelayMap -> ResPolMap -> Answer -> IO ()
          onAnswer packetH modAnsM ansM relM stoPol ansUR = 
                when b $ do case rID `M.lookup` relM of
                                Nothing -> pure ()
                                Just _ -> packetH . Right $ ans
                            when (maybe defaultStorePolitic id $ rID `M.lookup` stoPol) $ insertTO (ansValidity ans) modAnsM ansM (rID, ans)
                    where rID = cResID $ ansCert ans
                          ans = relayAnswer ansUR
                          b = case rID `lookupTO` ansM of
                                     Just a -> compareAnswer a ans 
                                     Nothing -> True 

          buildListenMap :: Event (RessourceID, Bool) -> Event Answer -> MomentIO (BehaviorC (EventMap RessourceID Answer))
          buildListenMap orderE ansE = do bhv <- newBehaviorMod M.empty 
                                          execute $ onOrder (bmModifier bhv) <$> orderE
                                          reactimate . filterJust $ apply (fireKey <$> bmLastValue bhv) ansE
                                          pure $ fmap eEvent <$> bmBhvC bhv
            where onOrder h (rID,b) = if b then (liftIO . h . M.insert rID) =<< newEventEntry 
                                           else liftIO . h $ M.delete rID


          relayAnswer :: Answer -> Answer
          relayAnswer a = a{ansTTL = ansTTL a - 1, ansRoad = uID : ansRoad a}
          relayResearch :: Research -> Research
          relayResearch res = res{resTTL = resTTL res - 1}
            -- TODO : renvoi True si la deuxième est mieux que la première
          compareAnswer :: Answer -> Answer -> Bool
          compareAnswer _ _ = True
          ansValidity :: Answer -> Time
          ansValidity = cResValidity . ansCert

sendAnswer :: DHPubKey -> PubKey -> UserID -> Handler RessourcePacket -> (Time, RawData) -> RessourceID -> IO ()
sendAnswer dhPK pK uID h (v,c) rID = do t <- getTime
                                        let cert = RessourceCert dhPK pK t v rID emptySignature
                                        h . Right $ Answer cert ttlMax [uID] uID c






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
                                    cResValidity :: Time,
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


