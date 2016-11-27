{-# LANGUAGE DeriveGeneric #-}
module Packets.Layer2 where

import Types.Crypto
import Types.Packets


import Data.Binary
import Control.Monad
import qualified Data.Map as M

import GHC.Generics

data L2 = L2Request Request | L2 Research Research | L2Answer Answer
    deriving Generic

data Request = Request {reqPosition :: Number, -- ^ Position on the road, changed during routing (NOT SIGNED)
                        reqLength :: Number, -- ^ Total length of the road
                        reqRoad :: Road,  -- ^ Road : list of UserID
                        reqDHPubKey :: DHPubKey,  -- ^ DHPubKey of the origin of the request
                        reqTime :: Time,    -- ^ Send time of the request
                        reqPipeKey :: PubKey, -- ^ Public Key of the opening pipe
                        reqPipeID  :: PipeID,  -- ^ PipeID of the pipe (KeyHash of the Public Key)
                        reqPipeSig :: Signature,  -- ^ Signature of the packet's Hash
                        reqContent :: RawData}   -- ^ extra content if needed (cause why not)
    deriving Generic

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


data RessourceCert = RessourceCert {cResSourceKey :: PubKey,
                                    cResTimestamp :: Time,
                                    cResValidity :: Time,
                                    cResID :: RessourceID,
                                    cResSig :: Signature}
                deriving Generic

instance Binary L2

instance Show Request where show (Request p l r _ t _ pID _ _) = "Request for pipe : " ++ show pID ++ " on road : " ++ show r ++" ("++ show p ++", "++ show l ++")"
instance SignedClass Request where scHash (Request n l r epk t pK pH s c) = encode (l,r,epk,t,pK,pH,c)
                                   scKeyHash = reqPipeID
                                   scSignature = reqPipeSig
                                   scPushSignature r s = r{reqPipeSig = s}
instance IntroClass Request where icPubKey = reqPipeKey
instance Binary Request

instance SignedClass Answer where scHash (Answer (RessourceCert k t t' rid _) _ _ sID r) = encode (k, t, t', rid, sID, r)
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


