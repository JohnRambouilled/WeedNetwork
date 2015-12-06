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
ansMaxDelay = 10

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


data Ressources = Ressources {resAnswerMap :: AnswerMapBhv,
                              resLocalAnswerMap :: LocalAnswerMapBhv,
                              resListenMap :: BehaviorC (EventMap RessourceID Answer),
                              resRelayMap :: RelayMapBhv,
                              resRelayPolitic :: ResPolMapBhv,
                              resStorePolitic :: ResPolMapBhv,
                              resListenHandler :: Handler (RessourceID, Bool),
                              resLogs :: Event String,
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
                                         (logsE, logsH) <- newEvent
                                         (listenE, listenH) <- newEvent
                                         (packetOE, packetOH) <- newEvent
                                         let onResB = onResearch logsH packetOH (bmModifier relB) <$> bmLastValue relB <*> bmLastValue ansB <*> bmLastValue locAnsB <*> bmLastValue relPol
                                             onAnsB = onAnswer logsH packetOH (bmModifier ansB) <$> bmLastValue relB <*> bmLastValue ansB <*> bmLastValue locAnsB <*> bmLastValue stoPol
                                         reactimate $ apply onResB resE
                                         reactimate $ apply onAnsB ansE
                                         listenB <- buildListenMap listenE ansE
                                         pure $ Ressources ansB locAnsB listenB relB relPol stoPol listenH logsE packetOE 

           {- Si la ressource est présente dans une des AnswerMap, on envoi la réponse. Sinon, on relay si elle est absente de la RelayMap et insert une entrée (time-out),
            - et on ignore si elle est présente dans la RelayMap -}
    where onResearch :: Handler String -> Handler RessourcePacket -> Modifier RelayMap -> RelayMap -> AnswerMap -> LocalAnswerMap -> ResPolMap -> Research -> IO () 
          onResearch logH packetH modRelM relM ansM locAnsM relPol res = when b $ case rID `M.lookup` locAnsM of
                        Just locA -> sendAnswer dhPK kP uID packetH locA rID
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
          onAnswer :: Handler String -> Handler RessourcePacket -> Modifier AnswerMap -> RelayMap -> AnswerMap -> LocalAnswerMap -> ResPolMap -> Answer -> IO ()
          onAnswer logH packetH modAnsM relM ansM locAnsM  stoPol ansUR = do
                eA <- checkAnswer uID ansUR
                case eA of
                    Left s -> logH s
                    Right ans -> when (b ans) $ 
                                    case rID `M.lookup` relM of
                                            Nothing -> logH "ressource not present in relay Map"
                                            Just _ -> do packetH . Right $ ans
                                                         logH "Relaying answer"
                                                         when (maybe defaultStorePolitic id $ rID `M.lookup` stoPol) $ do insertTO (ansValidity ans) modAnsM ansM (rID, ans)
                                                                                                                          logH "Storing answer for future uses"
                    where rID = cResID $ ansCert ansUR
                          b ans = case rID `M.lookup` locAnsM of
                                    Just _ -> False
                                    Nothing -> case rID `lookupTO` ansM of Just a -> compareAnswer ans a
                                                                           Nothing -> True
                          
                          
                          --maybe False id $ pure . compareAnswer ans <$> rID `lookupTO` ansM  <*> rID `M.lookup` locAnsM

          buildListenMap :: Event (RessourceID, Bool) -> Event Answer -> MomentIO (BehaviorC (EventMap RessourceID Answer))
          buildListenMap orderE ansE = do bhv <- newBehaviorMod M.empty 
                                          reactimate . (bmModifier bhv <$>) =<< execute (onOrder <$> orderE)
                                          reactimate . filterJust $ apply (fireKey <$> bmLastValue bhv) ansE
                                          pure $ fmap eEvent <$> bmBhvC bhv
            where onOrder :: (RessourceID, Bool) -> MomentIO (EventEntryMap RessourceID Answer -> EventEntryMap RessourceID Answer)
                  onOrder (rID,b) = if b then (pure . M.insert rID) =<< newEventEntry 
                                          else pure $ M.delete rID


          relayResearch :: Research -> Research
          relayResearch res = res{resTTL = resTTL res - 1}
            -- TODO : renvoi True si la premiere est mieux que la deuxieme
          compareAnswer :: Answer -> Answer -> Bool
          compareAnswer a a' = ansRoad a < ansRoad a'
            
          

          ansValidity :: Answer -> Time
          ansValidity = cResValidity . ansCert

sendAnswer :: DHPubKey -> KeyPair -> UserID -> Handler RessourcePacket -> (Time, RawData) -> RessourceID -> IO ()
sendAnswer dhPK kp uID h (v,c) rID = do t <- getTime
                                        let cert = RessourceCert dhPK (fst kp) t v rID emptySignature
                                        h . Right . sign kp $ Answer cert ttlMax [uID] uID c






checkAnswer :: UserID -> Answer -> IO (Either String Answer)
checkAnswer uID ans = checkAns uID <$> getTime <*> pure ans
    where checkAns me t ans@(Answer (RessourceCert _ pK ts val _ _) ttl r sID cnt)
            | ttl <= 1 || ttl > ttlMax      = Left "Incorrect TTL"
            | me `Prelude.elem` r          = Left "Already in the road" 
            | t - ts > val                 = Left "Answer obsolete"
            | computeHashFromKey pK /= sID  = Left "SourceID does not match PublicKey"
            | checkSig pK ans              = Right (relayAnswer ans)
            | otherwise                    = Left "Incorrect signature"
          relayAnswer :: Answer -> Answer
          relayAnswer a = a{ansTTL = ansTTL a - 1, ansRoad = uID : ansRoad a}

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


