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

relayTimeOut = 10 :: Time

defaultRelayPolitic = True
defaultStorePolitic = True
defaultListenPolitic = False

type LocalAnswer = (Time, RawData) --Durée de validité, et contenu

type ResPolMap = M.Map RessourceID Bool
type ResPolMapBhv t = ModEvent t ResPolMap

type AnswerMap = TimeMap RessourceID Answer --Map des Answers disponnibles
type AnswerMapBhv t = ModEvent t AnswerMap

type LocalAnswerMap = M.Map RessourceID LocalAnswer
type LocalAnswerMapBhv t = ModEvent t LocalAnswerMap

type AnswerEventMap = EventEntryMap RessourceID Answer --Map des Answers écoutées

type RelayMap = TimeMap RessourceID ()
type RelayMapBhv t = ModEvent t RelayMap 

--type LocalRessourceMap = M.Map RessourceID (Int, Time, RawData) --Nombre de répetition, et temps d'attente, contenue des réponses 
--genLocalResMap :: [RessourceID] -> LocalRessourceMap
--genLocalResMap rIDL = M.fromList $ f <$> rIDL
--    where f i = (i, (5, 5, encode i))



data Ressources t = Ressources {resAnswerMap :: AnswerMapBhv t,
                                resLocalAnswerMap :: LocalAnswerMapBhv t,
                                resListenMap :: Event t (EventMap RessourceID Answer),
                                resRelayMap :: RelayMapBhv t,
                                resRelayPolitic :: ResPolMapBhv t,
                                resStorePolitic :: ResPolMapBhv t,
                                resListenHandler :: Handler (RessourceID, Bool),
                                resRelPackets :: Event t RessourcePacket }

genResearch :: RessourceID -> RessourcePacket
genResearch rID = Left $ Research rID ttlMax [] emptyPayload

offerRessource :: Frameworks t => Ressources t -> (Time, RawData, RessourceID) -> IO ()
offerRessource res (t,d,rID) = meModifier (resLocalAnswerMap res) $ M.insert rID (t,d)

buildRessources :: Frameworks t => DHPubKey -> UserID -> KeyPair -> Event t RessourcePacket -> Moment t (Ressources t)
buildRessources dhPK uID kP packetE = do (resE, ansE) <- splitEither packetE
                                         [relPol, stoPol] <- forM [1..2] $ pure $ newModEvent M.empty
                                         ansB <- newModEvent M.empty
                                         locAnsB <- newModEvent M.empty
                                         relB <- newModEvent M.empty
                                         (listenE, listenH) <- newEvent
                                         (packetOE, packetOH) <- newEvent
                                         let onResB = onResearch packetOH (meModifier relB) <$> meLastValue relB <*> meLastValue ansB <*> meLastValue locAnsB <*> meLastValue relPol
                                             onAnsB = onAnswer packetOH (meModifier ansB) <$> meLastValue ansB <*> meLastValue relB <*> meLastValue stoPol
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

          buildListenMap :: Frameworks t => Event t (RessourceID, Bool) -> Event t Answer -> Moment t (Event t (EventMap RessourceID Answer))
          buildListenMap orderE ansE = do bhv <- newModEvent M.empty 
                                          reactimate $ onOrder (meModifier bhv) <$> orderE
                                          reactimate . filterJust $ apply (fireKey <$> meLastValue bhv) ansE
                                          pure $ (eAddHandler <$>) <$> meChanges bhv
            where onOrder h (rID,b) = if b then (h . M.insert rID) =<< newEventEntry (pure True)
                                           else h $ M.delete rID


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
              sendAnswer :: Handler Answer -> (Time, RawData) -> RessourceID -> IO ()
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

-}
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


