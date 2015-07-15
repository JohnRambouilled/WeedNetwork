{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Client.Ressource where


import Client.Class
import Client.Crypto
import Client.Packet
import Client.Neighborhood
import Log

import Control.Monad.State hiding (get,put)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Bifunctor
import Control.Concurrent.MVar
import Data.Binary
import Data.ByteString.Lazy hiding (null,head,tail)
import Data.Maybe
import Data.Time hiding (DiffTime)
import Data.Tuple
import Timer
import Crypto.Random

type TTL = Int
newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord)



data RessourceCert = RessourceCert {cResSourceDHKey :: DHPubKey,
                                    cResSourceKey :: PubKey,
                                    cResTimestamp :: Time,
                                    cResID :: RessourceID,
                                    cResSig :: Sig}

data RessourcePacket = Research {resID :: RessourceID,
                                 resTTL :: TTL,
                                 resRoad :: Road,
                                 resCnt :: RawData}
                     | Answer {resCert :: RessourceCert,
                               resTTL :: TTL,
                               resRoad :: Road,
                               resSourceID :: SourceID,
                               resCnt :: RawData}


instance Show RessourcePacket where
        show (Research rID ttl r _ ) = "Research : rID = " ++ show rID ++ " ttl = " ++ show ttl ++ " road = " ++ show r
        show (Answer rCert ttl r sID _ ) = "Answer : rID = " ++ show (cResID rCert) ++ " ttl = " ++ show ttl ++ " source = " ++ show sID ++ " road = " ++ show r



ressourceTtlMax :: Int
ressourceTtlMax = 10


isResearch (Research _ _ _ _) = True
isResearch _  = False
isAnswer = not . isResearch

data RessourceEntry = RessourceEntry {runRessourceEntry :: [RessourceCB]}

type RessourceModule = MapModule RessourceEntry RessourceID RessourcePacket RessourcePacket
type RessourceCB = Behaviour RessourceModule RessourcePacket IO [RessourcePacket]


instance MapModules RessourceEntry RessourceID RessourcePacket RessourcePacket where
        packetKey = ask >>= packetKey'
            where packetKey' (Research rID _ _ _) = do keepLog RessourcesLog Normal $ "research for : " ++ show rID
                                                       pure . Just $ rID
                  packetKey' (Answer crt _ _ _  _) = do keepLog RessourcesLog Normal $ "Answer for : " ++ show (cResID crt)
                                                        pure . Just $ cResID crt
        entryBehaviour b = [joinMapBehaviour $ runRessourceEntry b]






{- The default callback consists of repeating it -}

ressourceTimeOut = 200 :: DiffTime -- TODO

runRessourceModule :: (MonadIO m) => MVar RessourceModule -> MVar Timer -> SourceID -> Behaviour RessourceModule RessourcePacket m ()
runRessourceModule ress timer me = do ask >>= \packet -> modifyDefaultMapBehaviour $ \b -> (newDefaultBehaviour ress timer me packet):b

ressourceDefaultBehaviour ::  MVar RessourceModule -> MVar Timer -> SourceID -> RessourceCB
ressourceDefaultBehaviour r t m = ask >>= newDefaultBehaviour r t m

newDefaultBehaviour :: MVar RessourceModule -> MVar Timer -> SourceID -> RessourcePacket ->  RessourceCB
newDefaultBehaviour ress timer me pkt@(Research rID ttl road cnt)  = if ttl > 1  && ttl < ressourceTtlMax then do 
                                                                                        (refresh,_) <- liftIO (registerTimerM timer ressourceTimeOut (unregisterM ress rID >> pure True))
                                                                                        insertMapBehaviour rID (entry refresh)
                                                                                        pure . maybeToList $ relayResearchPacket pkt else return []
  where entry refresh = RessourceEntry [ask >>= \pkt -> if isAnswer pkt then liftIO refresh >> (pure . maybeToList $ relayAnswerPacket me pkt) else return []]
        unreg = unregisterM ress rID
newDefaultBehaviour _ _ _ _ = return []


checkCert :: RessourceCert -> SourceID -> Bool 
checkCert (RessourceCert dhKey sKey time rID sig) (SourceID kH)  = runKeyHash kH  == pubKeyToHash sKey && (checkSignature sKey sig $ encode (dhKey, sKey, time, rID))


{-| Accepts only researches if they have an empty road or a road where my user ID is on its top.
    Accepts only answers if they have a properly signed certificate. |-}

ressourceHashFunction me = ask >>= ressourceHashFunction' me
ressourceHashFunction' :: SourceID -> Packet -> HashFunction RessourcePacket
ressourceHashFunction' me (DataPacket kID _ dc@(DataContent cnt)) = do 
        keepLog RessourcesLog Normal $ "new Ressource packet"
        case decodeMaybe cnt of Just pkt@(Research rID ttl road cnt) -> if ressourceTtlMax >= ttl && ttl > 0 && checkRoad road then pure . Just $ (encode dc,pkt) 
                                                                                                                               else do keepLog RessourcesLog Normal "invalid Research"
                                                                                                                                       pure Nothing
                                Just pkt@(Answer crt ttl roadL source cnt) -> if ressourceTtlMax >= ttl && ttl > 0 && checkCert crt source then pure . Just $ (encode dc, pkt)
                                                                                    else do keepLog RessourcesLog Normal "invalid Answer"
                                                                                            pure Nothing
                                Nothing -> (keepLog RessourcesLog Normal "invalid RessourcePacket") >> pure Nothing
        where checkRoad road = if null road then True else me == head road
ressourceHashFunction' _ _ = return Nothing

relayResearchPacket:: RessourcePacket -> Maybe RessourcePacket
relayResearchPacket (Research rID ttl road cnt) = if ttl > 1 then Just $ Research rID (ttl - 1) road' cnt
                                                                else Nothing
          where road' = if null road then [] else tail road
relayResearchPacket (Answer crt ttl roadL source cnt) = Nothing 

-- TODO est-ce qu'il faut checker le certificat ? La hashfunction le fait déjà
relayAnswerPacket me (Answer crt ttl roadL source cnt) = if ttl > 1 && not (me `Prelude.elem` roadL)
                                                           then Just $ Answer crt (ttl-1) (me:roadL) source cnt
                                                           else Nothing
relayAnswerPacket _ _ = Nothing


registerRessourceModule :: SourceID -> PrivKey -> PubKey -> MVar RessourceModule -> DataCB
registerRessourceModule me uK pK mv = DataCB (ressourceHashFunction me) clbk --(ressourceHashFunction,clbk)
  where clbk :: CryptoCB RessourcePacket [Packet]
        clbk = ask >>= \rP -> join $ (uncurry (>>) . swap . bimap (mapM craftPacket) tell) <$> (liftIO $ runModule mv rP) 
        craftPacket :: (MonadIO m,MonadState Crypto m) => RessourcePacket ->  m Packet
        craftPacket resPkt = return $ DataPacket (keyHash me) (sign uK pK $ encode dc) dc
                                  where dc = DataContent $ encode resPkt

 
sendResearch :: PrivKey -> PubKey -> KeyHash -> RessourceID -> TTL ->Road -> RawData -> Packet
sendResearch prK pK uID rID ttl r d = let res = DataContent. encode $ Research rID ttl r d
                                  in DataPacket uID (sign prK pK $ encode res) res


answerMaxFrequency = 100 :: DiffTime

genRessourceCallback :: MVar Time -> PubKey -> PrivKey -> SourceID -> RessourceID -> RawData -> RessourceCB
genRessourceCallback tV pK uK uID rID d = do p <- ask
                                             if isResearch p then do keepLog RessourcesLog Important $ "received research for offered ressource : " ++ show rID
                                                                     tM <- liftIO $ tryReadMVar tV
                                                                     t' <- liftIO getTime
                                                                     case tM of Nothing -> do liftIO $ putMVar tV t' 
                                                                                              pure [sendAnswer t'] 
                                                                                Just t -> if diffUTCTime t' t > answerMaxFrequency then do liftIO $ swapMVar tV t'
                                                                                                                                           pure [sendAnswer t]
                                                                                                                                     else pure []
                                                           else pure []
    where sendAnswer t = Answer (cert t) ressourceTtlMax [uID] uID d
          cert t = RessourceCert dhKey pK t rID $ sign uK pK $ encode (dhKey, pK, t, rID)
          dhKey = fromJust $ privKeyToDHPubKey uK

                             

instance Show RessourceID where show (RessourceID d) = "RID:"++ prettyPrint d

instance Binary RessourceID where put (RessourceID rID) = put rID
                                  get = RessourceID <$> get

instance Binary RessourceCert where put (RessourceCert dh key time rID sig) = put dh >> put key >> put time >> put rID >> put sig
                                    get = RessourceCert <$> get <*> get <*> get <*> get <*> get
instance Binary RessourcePacket where 
        put (Research rID ttl road cnt) = putWord8 1 >> put rID >> put ttl >> put road >> put cnt
        put (Answer crt ttl road source cnt) = putWord8 2 >> put crt >> put ttl >> put road >> put source >> put cnt

        get = do w <- getWord8
                 case w of
                   1 -> Research <$> get <*> get <*> get <*> get
                   2 -> Answer <$>  get <*> get <*> get <*> get <*> get
                   otherwise -> fail "invalid type of ressource packet"
