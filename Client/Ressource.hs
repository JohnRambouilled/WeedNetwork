{-# LANGUAGE RankNTypes #-}
module Client.Ressource where

import Packets
import Types
import Client.Crypto
import Client.Timer
import Client.WeedMonad
import Client.Sender
import Client.Pipes
--import Types.Graph.RoadGraph
import Types.Graph.Type
import Types.Graph.Search
import Types.Graph.RoadGraph


import Data.Ord
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import qualified Data.Map as M

answerMaxTTL = 10 :: TTL
ressourceEntryTiltTime = 3 :: Time
answerValidity = 15 :: Time
answerRoadSearchDepth = 10 :: Int
ressourceSourceTimeOut = 60 :: Time
researchMaxTTL = 10 :: TTL


offerRessource :: RessourceID -> RawData -> WeedMonad ()
offerRessource rID d = do resMap <-  stmRead clRessources
                          case rID `M.lookup` resMap of
                            Just (RessourceOffered _ _ _) -> stmModify clRessources $ M.adjust (set ressourceAnswerContent d) rID
                            Just (RessourceResearched  m t1 t2) -> do mapM killTimer $ M.elems m
                                                                      killTilt t1 >> killTilt t2
                                                                      insertNewRessource
                            Nothing -> insertNewRessource
  where insertNewRessource :: WeedMonad()
        insertNewRessource = stmModify clRessources . M.insert rID =<< newRessourceOffered rID d
        killTilt = killTimer . _tiltTimer

researchSimpleRessource :: RessourceID -> WeedMonad ()
researchSimpleRessource rID = researchRessource rID researchMaxTTL [] emptyPayload
  
researchRessource :: RessourceID -> TTL -> Road -> RawData -> WeedMonad ()
researchRessource rID ttl r d = do resMap <- stmRead clRessources
                                   case rID `M.lookup` resMap of
                                      Just (RessourceOffered _ _ _) -> do logM "Client.Ressource" "researchRessource" Error $ "call for a ressource already offered :  " ++ show rID
                                      Just (RessourceResearched _ _ t) -> unless' t $ do tiltResearch rID
                                                                                         sendResearch rID ttl r d
                                      Nothing -> do e <- newRessourceResearched rID []
                                                    stmModify clRessources $ M.insert rID e
                                                    tiltResearch rID
                                                    sendResearch rID ttl r d
   where unless' = unless . view tiltOn                                   
                                                    
                                                    

onAnswer :: Answer -> WeedMonad ()
onAnswer ans = do (uID, resMap) <- (,) <$> (clUserID <$> getClient) <*> stmRead clRessources
                  t <- getTime
                  case checkAnswer uID t ans of
                        Left s -> logM "Client.Ressource" "onAnswer" InvalidPacket s
                        Right rel -> case M.lookup rID resMap of
                                        Just (RessourceResearched m t _) -> unless' t $ do newResearchedAnswer m ans 
                                                                                           tiltAnswer rID
                                                                                           relayAnswer ans
                                        _ -> pure ()
    where rID = view (ansCert . cResID) ans
          unless' = unless . view tiltOn
                    

newResearchedAnswer :: M.Map SourceID TimerEntry -> Answer -> WeedMonad ()
newResearchedAnswer m ans = do me <- keyHash2VertexID . clUserID <$> getClient
                               logM "Client.Ressource" "newRessourceResearched" Normal $ "Answer for a researched ressource : " ++ show rID ++ " received from : " ++ show sourceID
                               t <- getTime
                               stmModify clGraph . addRoad (me, mempty) $ map (wrap t) (_ansRoad ans)
                               case sourceID `M.lookup` m of
                                 Just t -> refreshTimer t
                                 Nothing -> overSourceMap . M.insert sourceID =<< newTimerEntry ressourceSourceTimeOut (overSourceMap $ M.delete sourceID)
   where rID = view (ansCert . cResID) ans
         wrap t uID = ((keyHash2VertexID uID, EdgeT t), mempty)
         sourceID = last $ view ansRoad ans
         overSourceMap :: (M.Map SourceID TimerEntry -> M.Map SourceID TimerEntry) -> WeedMonad ()
         overSourceMap f = stmModify clRessources $ M.adjust (over ressourceSources f) rID
         
                             


-- | Check the research, and manages it : 
-- |        -> if the ressource is unknown, a new entry is created and the research is relayed (tilt is ON)
-- |        -> if the ressource if offered, and answer is forged ans sent (unless tilt is ON). Tilt is switched on.
-- |        -> if the ressource is already researhed (unless tilt is ON) :  -> if we don't now any source offering the research, we relay the answer. tilt is switched on.
-- |                                                                        -> if we do, we call sendRemoteAnswer to forge and send the corresponding answer. tilt is switched on.
onResearch :: Research -> WeedMonad ()
onResearch res = do (uID, resMap) <- (,) <$> (clUserID <$> getClient) <*> stmRead clRessources
                    case checkResearch uID res of
                        Left s -> logM "Client.Ressource" "onResearch" InvalidPacket s
                        Right rel -> case M.lookup rID resMap of
                                        Nothing -> do stmModify clRessources =<< (M.insert rID <$> newRessourceResearched rID []  )
                                                      tiltResearch rID
                                                      logM "Client.Ressource" "onRessearch" Normal $ "New ressource researched : " ++ show rID 
                                                      when rel $ relayResearch res
                                        Just (RessourceOffered c _ t) -> unless' t $ do logM "Client.Ressource" "onRessearch" Normal $ "Sending answer for Ressource : " ++ show rID 
                                                                                        sendAnswer rID answerValidity answerMaxTTL c
                                                                                        tiltResearch rID
                                        Just (RessourceResearched map _ t) -> unless' t $ tiltResearch rID >> if M.null map then relayResearch res
                                                                                                              else sendRemoteAnswer rID $ M.keys map
    where unless' = unless . view tiltOn 
          rID = _resID res

lookupSources :: RessourceID -> WeedMonad [SourceID]
lookupSources rID = do rMap <- stmRead clRessources
                       case rID `M.lookup` rMap of
                         Just (RessourceResearched sMap _ _ ) -> pure $ M.keys sMap
                         _ -> pure []

-- | Lookup in the graph to find roads leading to the given sources, and send an answer for the specified ressource
sendRemoteAnswer :: RessourceID -> [SourceID] -> WeedMonad () 
sendRemoteAnswer rID sIDs = do t <- getTime
                               roads <- lookupRoads sIDs
                               mapM_ (sendAns t) $ choose roads
   where sendAns t r = sendAnswer rID t answerMaxTTL emptyPayload
         choose x | null x = []
                  | otherwise = minimumBy (comparing length)$ x

 
lookupRoads :: [SourceID] -> WeedMonad [Road] 
lookupRoads sIDs = do me <- keyHash2VertexID . clUserID <$> getClient
                      map (map vertexID2KeyHash) . catMaybes <$> forM sIDs (search me . keyHash2VertexID)
   where search :: VertexID ->  VertexID -> WeedMonad (Maybe [VertexID])
         search me dest = fmap (map fst) <$> (searchRoad me dest answerRoadSearchDepth =<< stmRead clGraph)


vertexID2KeyHash :: VertexID -> UserID
vertexID2KeyHash (VertexID u) = KeyHash u 

tiltAnswer :: RessourceID -> WeedMonad ()
tiltAnswer = tilt False

tiltResearch :: RessourceID -> WeedMonad ()
tiltResearch = tilt True

tilt :: Bool -> RessourceID -> WeedMonad ()
tilt isRes rID = do m <- stmRead clRessources
                    case view (accessor isRes) <$> M.lookup rID m of 
                        Nothing -> logM "Client.Ressource" "tiltRessource" Error "Tried to tilt an absent ressource."
                        Just e -> if _tiltOn e then logM "Client.Ressource" "tiltRessource" Error "Tilt was already on"
                                  else startTimer (_tiltTimer e) >> setOn
    where accessor i = if i then researchTilt else answerTilt
          setOn = stmModify clRessources $ M.adjust (set (accessor isRes . tiltOn) True) rID

newTilt :: RessourceID -> Lens' RessourceEntry RessourceTilt -> WeedMonad RessourceTilt
newTilt rID l = RessourceTilt False <$> timer l
  where timer l = newTimerEntry ressourceEntryTiltTime $ setOff l 
        setOff l = stmModify clRessources $ M.adjust (set (l . tiltOn) False) rID

newRessourceOffered :: RessourceID -> RawData -> WeedMonad RessourceEntry
newRessourceOffered rID d = RessourceOffered d <$> newTilt rID answerTilt <*> newTilt rID researchTilt

newRessourceResearched :: RessourceID -> [SourceID] -> WeedMonad RessourceEntry
newRessourceResearched rID sL = RessourceResearched <$> (M.fromList <$> mapM newSource sL) <*> newTilt rID answerTilt <*> newTilt rID researchTilt
    where newSource sID = (,) sID <$> newTimerEntry_

checkResearch :: UserID -> Research -> Either String Bool
checkResearch me res@(Research rID ttl r c)
    | ttl > researchMaxTTL             = Left "TTL exceeding maximum allowed ttl"
    | ttl < 0                          = Left "Negative TTL"
    | not (null r) && (me /= head r)    = Left "Research not adressed to me"
    | otherwise                        = Right (ttl > 0)


checkAnswer :: UserID -> Time -> Answer -> Either String Bool
checkAnswer me t ans@(Answer (RessourceCert pK ts val _ _) ttl r sID cnt)
            | ttl < 0 || ttl > answerMaxTTL = Left "Incorrect TTL"
            | me `Prelude.elem` r           = Left "Already in the road" 
            | t - ts > val                  = Left "Answer obsolete"
            | computeHashFromKey pK /= sID  = Left "SourceID does not match PublicKey"
            | checkSig pK ans               = Right (ttl > 0)
            | otherwise                     = Left "Incorrect signature"


