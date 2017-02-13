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


import Data.Ord
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import qualified Data.Map as M

researchMaxTTL = 10 :: TTL
answerMaxTTL = 10 :: TTL
ressourceEntryTiltTime = 3 :: Time
answerValidity = 15 :: Time
answerRoadSearchDepth = 10 :: Int




onAnswer :: Answer -> WeedMonad ()
onAnswer ans = do (uID, resMap) <- (,) <$> (clUserID <$> getClient) <*> stmRead clRessources
                  t <- getTime
                  case checkAnswer uID t ans of
                        Left s -> logM "Client.Ressource" "onAnswer" InvalidPacket s
                        Right rel -> case M.lookup rID resMap of
                                        Just (RessourceResearched m t _) -> unless' t $ do newResearchedAnswer ans 
                                                                                           tiltAnswer rID
                                                                                           relayAnswer ans
                                        _ -> pure ()
    where rID = view (ansCert . cResID) ans
          unless' = unless . view tiltOn
                    

newResearchedAnswer :: Answer -> WeedMonad ()
newResearchedAnswer ans = do me <- keyHash2VertexID . clUserID <$> getClient
                             stmModify clGraph . addRoad (me, mempty) $ map wrap (_ansRoad ans)
   where wrap uID = ((keyHash2VertexID uID, mempty), mempty)
                             


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


-- | Lookup in the graph to find roads leading to the given sources, and send an answer for the specified ressource
sendRemoteAnswer :: RessourceID -> [SourceID] -> WeedMonad () 
sendRemoteAnswer rID sIDs = do me <- keyHash2VertexID . clUserID <$> getClient
                               t <- getTime
                               roads <- map (map vertexID2KeyHash) . catMaybes <$> forM sIDs (search me . keyHash2VertexID)
                               mapM_ (sendAns t) $ choose roads
   where search :: VertexID ->  VertexID -> WeedMonad (Maybe [VertexID])
         search me dest = fmap (map fst) <$> (searchRoad me dest answerRoadSearchDepth =<< stmRead clGraph)
         sendAns t r = sendAnswer rID t answerMaxTTL emptyPayload
         choose x | null x = []
                  | otherwise = minimumBy (comparing length)$ x

 

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



newRessourceResearched :: RessourceID -> [SourceID] -> WeedMonad RessourceEntry
newRessourceResearched rID sL = RessourceResearched <$> (M.fromList <$> mapM newSource sL) <*> newTilt answerTilt <*> newTilt researchTilt
    where newSource sID = (,) sID <$> newTimerEntry_
          newTilt l = RessourceTilt False <$> timer l
          timer l = newTimerEntry ressourceEntryTiltTime $ setOff l 
          setOff l = stmModify clRessources $ M.adjust (set (l . tiltOn) False) rID


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
            | computeHashFromKey pK /= sID   = Left "SourceID does not match PublicKey"
            | checkSig pK ans               = Right (ttl > 0)
            | otherwise                     = Left "Incorrect signature"


