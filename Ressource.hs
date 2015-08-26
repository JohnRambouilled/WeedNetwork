{-# LANGUAGE DeriveGeneric #-}
module Ressource where

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

data AnswerOrder = AnswerAdd RessourceID (Handler Answer)
                 | AnswerDel RessourceID 
data ResearchOrder = ResearchAdd RessourceID (Handler Research)
                   | ResearchDel RessourceID 

-- Ce que l'on doit faire des answers pour chaque ressourceID
type AnswerMap = M.Map RessourceID (Handler Answer)
type ResearchMap = M.Map RessourceID (Handler Research)

type RessourceMap = M.Map KeyHash (Event Research, Event Answer)

-- Répartition des ressources selon les sourcesID
--type RessourceManager = M.Map SourceID [RessourceID] -- TODO

runRessourceModule :: SourceID -> (Behaviour CryptoMap) -> Reactive (Behaviour RessourceMap,
                                                                  (Behaviour ResearchMap, Handler ResearchOrder),
                                                                  (Behaviour AnswerMap, Handler AnswerOrder))
runRessourceModule me cMapB  = do ((resOrder,fireRes),(ansOrder,fireAns)) <- (,) <$> newEvent <*> newEvent
                                  resMapB <- accum M.empty $ onResearchOrder <$> resOrder
                                  ansMapB <- accum M.empty $ onAnswerOrder <$> ansOrder

                                  -- listen des recherches/réponses
                                  listenTrans (onResearch fireRes fireAns resInputE resMapB) id
                                  listenTrans (onAnswer ansInputE ansMapB) id
                                  
                                  pure (buildRessourceMap me <$> cMapB,
                                        (resMapB,fireRes),
                                        (ansMapB,fireAns))

  where  
         extractMaps :: CryptoMap -> (Event Research, Event Answer)
         extractMaps cMap =  ((,) <$> extractStream . fmap fst <*> extractStream . fmap snd) $ buildRessourceMap me cMap
         extractStream :: (M.Map k (Event a)) -> Event a
         extractStream =Prelude.foldr merge never . M.elems
         (resInputE,ansInputE) = ((,) <$> switchE . fmap fst <*> switchE . fmap snd) $ (extractMaps <$> cMapB) 


onAnswer :: Event Answer -> Behaviour AnswerMap -> Event (Reactive ())
onAnswer ansE rMapB = filterJust $ snapshot onAnswer' ansE rMapB
        where onAnswer' :: Answer -> AnswerMap -> Maybe (Reactive () )
              onAnswer' ans rMap = fmap ($ans) $ (cResID $ ansCert ans) `M.lookup` rMap 


onAnswerOrder :: AnswerOrder -> AnswerMap -> AnswerMap 
onAnswerOrder (AnswerAdd rID rE) resM = M.insert rID rE resM 
onAnswerOrder (AnswerDel rID) resM = M.delete rID resM 
onResearchOrder (ResearchAdd rID rE) = M.insert rID rE
onResearchOrder (ResearchDel rID) = M.delete rID


buildRessourceMap :: SourceID -> CryptoMap -> RessourceMap
buildRessourceMap me cMap = fmap (over _2 checkStream . extractRessourceStream) cMap
  where extractRessourceStream :: Event DataPacket -> (Event Research, Event Answer)
        extractRessourceStream dataE = filterEither $ filterJust $ decodeRessource <$> dataE
        decodeRessource :: DataPacket -> Maybe (Either Research Answer)
        decodeRessource dataPkt = case decodeOrFail (dataSignedPayload dataPkt) of
                                    Right (_,_,res) -> Just $ Left res
                                    Left (_,_,_) -> case decodeOrFail (dataSignedPayload dataPkt) of
                                                      Right (_,_,ans) -> Just $ Right ans
                                                      Left _ -> Nothing
        checkStream :: Event Answer -> Event Answer
        checkStream  ansE = snd <$> filterE (uncurry $ checkAnswer me) (timeEvent ansE)

onResearch :: Handler ResearchOrder -> Handler AnswerOrder -> Event Research -> Behaviour ResearchMap -> Event (Reactive ())
onResearch resOrder ansOrder resE resB = snapshot onResearch' resE resB
  where onResearch' :: Research -> ResearchMap -> Reactive ()
        onResearch' res rMap = case resID res `M.lookup` rMap of
                                 Just h -> h res
                                 Nothing -> do resOrder $ ResearchAdd (resID res) $ pure (pure ())
                                               ansOrder $ AnswerAdd (resID res) $ relayAnswer
                                               relayResearch res
        relayAnswer :: Handler Answer
        relayAnswer _ = pure () --TODO
        relayResearch :: Handler Research
        relayResearch _ = pure () -- TODO

checkCert source time (RessourceCert dhKey pKey sendTime rID sig) = time - sendTime < maxDelay
                                                                    && computeHashFromKey pKey == source
                                                                    && checkSig pKey sig (encode (dhKey,pKey,sendTime,rID))
checkAnswer me time ans = ansTTL ans > 1 && ansTTL ans <= ttlMax &&
                          me `Prelude.notElem` ansRoad ans && checkCert (ansSourceID ans) time (ansCert ans)






instance Binary RessourceID
instance Binary RessourceCert
instance Binary Research
instance Binary Answer

