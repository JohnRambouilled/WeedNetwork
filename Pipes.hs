{-# LANGUAGE DeriveGeneric #-}

module Pipes where

import FRP.Sodium
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M

type RawData = ByteString
type Number = Int
type SourceID = Int
type PubKey = Int
type KeyHash = Int
type Signature = RawData
type Road = [SourceID]
type Time = Int
type PipeID = KeyHash

data Request = Request {reqPosition :: Number,
                        reqLength :: Number, -- ^ Total length of the road
                        reqRoad :: Road,  -- ^ Road : list of UserID
                        reqEPK :: RawData,  -- ^ encrypted (Keyhash, PrivKey) for the destination of the road
                        reqTime :: Time,
                        reqPipeKey :: PubKey,
                        reqPipeID  :: KeyHash,
                        reqPipeSig :: Signature,
                        reqContent :: RawData}
    deriving Generic


data PipeMessage = PipeData {messageDirection :: Bool,
                             messageContent :: RawData} |
                   PipeExit {messageDirection :: Bool,
                             messageContent :: RawData} 
    deriving Generic

data PipeOrder = PipeAdd PipeID PipeEntry
               | PipeDel PipeID

isPipeExit (PipeExit _ _) = True
isPipeExit _ = False

isPipeData = not . isPipeExit

instance Binary Request
instance Binary PipeMessage


type PipeEntry = Event (PipeID,PipeMessage)
type PipeMap = M.Map PipeID PipeEntry

onExit :: (PipeID,PipeMessage) -> PipeMap -> PipeMap
onExit (pID,pMessage) pMap = if isPipeExit pMessage then M.delete pID pMap else pMap

onRequest :: Event (PipeID,PipeMessage) -> Request -> PipeMap -> PipeMap
onRequest pMsgsE pReq pMap = case pID `M.lookup` pMap of
                                     Just _ -> pMap -- TODO refresh du pipe
                                     Nothing -> M.insert pID (filterE ((== pID) . fst) pMsgsE) pMap
       where pID = reqPipeID pReq

isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft

newPipeModule' :: Event Request -- Request provenant de la source considérée
              -> Event (PipeID,PipeMessage) -- PipeMessages 
              -> Reactive (Behaviour (Event (PipeID,PipeMessage))) -- Flux vérifié des pipesData (actualisé en fonction des nouveaux pipes ouverts)
newPipeModule' reqE pMsgsE  = do 
                                pMapB <- accum M.empty $ onDataMsg <$> dataE
                                pure $ eventL <$> pMapB
  where dataE = (Left <$> reqE) `merge` (Right <$> pMsgsE)
        onDataMsg (Left req) = onRequest pMsgsE req
        onDataMsg (Right pMsg) = onExit pMsg
        eventL pMap = filterE (isPipeData . snd) $ foldr merge never (M.elems pMap)

{-| En provenance d'une source donnée |-}
newPipeModule :: Event Request -- Les requêtes qu'elle nous envoie
              -> Event (PipeID,PipeMessage) -- Les pipemessage qu'elle nous envoie (tout pipeID confondu)
              -> Reactive (Event (PipeID,PipeMessage)) -- L'event actuel de tout les pipesMessages qui proviennent de la source (en fonction des pipes courants)
newPipeModule reqE pMsgE = switchE <$> newPipeModule' reqE pMsgE

