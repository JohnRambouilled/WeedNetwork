module Client.Ressource where

import Packets
import Types
import Client.Crypto
import Client.Timer
import Client.WeedMonad
import Client.Sender

import Control.Lens
import Control.Monad
import qualified Data.Map as M

researchMaxTTL = 10 :: TTL
answerMaxTTL = 10 :: TTL
ressourceEntryTiltTime = 3 :: Time
answerValidity = 15 :: Time




onAnswer :: Answer -> WeedMonad ()
onAnswer ans = do (uID, resMap) <- (,) <$> (clUserID <$> getClient) <*> stmRead clRessources
                  t <- getTime
                  case checkAnswer uID t ans of
                        Left s -> logM "Client.Ressource" "onAnswer" InvalidPacket s
                        Right rel -> case M.lookup rID resMap of
                                        Just (RessourceResearched m t b) -> unless' b $ do newResearchedAnswer ans 
                                                                                           tiltAnswer rID
                                                                                           relayAnswer ans
                                        _ -> pure ()
    where rID = view (ansCert . cResID) ans
          unless' t = unless (_resTiltAns t)
                    

newResearchedAnswer :: Answer -> WeedMonad ()
newResearchedAnswer = undefined


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
                                        Nothing -> do stmModify clRessources =<< (M.insert rID <$> newRessourceResearched rID [] (RessourceTilt False True) )
                                                      logM "Client.Ressource" "onRessearch" Normal $ "New ressource researched : " ++ show rID 
                                                      when rel $ relayResearch res
                                        Just (RessourceOffered c t b) -> unless' b $ do logM "Client.Ressource" "onRessearch" Normal $ "Sending answer for Ressource : " ++ show rID 
                                                                                        sendAnswer rID answerValidity answerMaxTTL c
                                                                                        tiltResearch rID
                                        Just (RessourceResearched map t b) -> unless' b $ tiltResearch rID >> if M.null map then relayResearch res
                                                                                                              else sendRemoteAnswer rID $ M.keys map
    where unless' t = unless (_resTiltRes t)
          rID = _resID res


-- | Lookup in the graph to find roads leading to the given sources, and send an answer for the specified ressource
sendRemoteAnswer :: RessourceID -> [SourceID] -> WeedMonad () 
sendRemoteAnswer rID sIDs = undefined  -- [TODO]


tiltAnswer :: RessourceID -> WeedMonad ()
tiltAnswer = tilt False

tiltResearch :: RessourceID -> WeedMonad ()
tiltResearch = tilt True

tilt :: Bool -> RessourceID -> WeedMonad ()
tilt isRes rID = do m <- stmRead clRessources
                    case M.lookup rID m of Nothing -> logM "Client.Ressource" "tiltRessource" Error "Tried to tilt an absent ressource."
                                           Just e -> do startTimer (_ressourceTimer e) ressourceEntryTiltTime $ setF False
                                                        setF True
    where accessor = ressourceTilt . (if isRes then resTiltRes else resTiltAns)
          setF b = stmModify clRessources $ M.adjust (set accessor b) rID
                

newRessourceResearched :: RessourceID -> [SourceID] -> RessourceTilt -> WeedMonad RessourceEntry
newRessourceResearched rID sL (RessourceTilt a r) = do res <- RessourceResearched <$> (M.fromList <$> mapM newSource sL) <*> createTimerEntry <*> pure (RessourceTilt False False)
                                                       when a $ tiltAnswer rID 
                                                       when r $ tiltResearch rID
                                                       pure res
    where newSource sID = (,) sID <$> createTimerEntry


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


