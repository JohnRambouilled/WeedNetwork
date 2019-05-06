{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell   #-}
module Packets.Layer2 where

import Types.Crypto
import Types.Packets


import Data.Binary
import Control.Monad
import Control.Lens
import qualified Data.ByteString.Lazy  as B
import qualified Data.Map as M
import GHC.Generics

data L2 = L2Request Request | L2Research Research | L2Answer Answer 
    deriving (Generic, Show)


data Request = Request {_reqPosition :: Number, -- ^ Position on the road, decrease during routing (NOT SIGNED)
                        _reqRoad :: Road,  -- ^ Road : list of UserID. Head is the destinary of the request, and the last element the source. 
                        _reqSourceKey :: PubKey,  -- ^ DHPubKey of the origin of the request
                        _reqTime :: Time,    -- ^ Send time of the request
                        _reqPipeKey :: PipePubKey, -- ^ Public Key of the opening pipe
                        _reqPipeID  :: PipeID,  -- ^ PipeID of the pipe (KeyHash of the Road)
                        _reqPipeSig :: Signature,  -- ^ Signature of the packet's Hash
                        _reqContent :: RawData}   -- ^ extra content if needed (cause why not)
    deriving Generic


data Research = Research {_resID :: RessourceID,  -- ^ Ressource researched
                          _resTTL :: TTL,         -- ^ number of time the research will be relayed yet.
                          _resRoad :: Road,       -- ^ Road along wich the research should be propagated.
                          _resCnt :: RawData}    -- ^ Research content
                deriving Generic

data Answer = Answer {_ansCert :: RessourceCert,
                      _ansTTL :: TTL,
                      _ansRoad :: Road,
                      _ansSourceID :: SourceID,
                      _ansCnt :: RawData}
                deriving Generic


data RessourceCert = RessourceCert {_cResSourceKey :: PubKey,
                                    _cResTimestamp :: Time,
                                    _cResValidity :: Time,
                                    _cResID :: RessourceID,
                                    _cResSig :: Signature}
                deriving Generic

instance Show Request where show (Request p r _ t _ pID _ _) = "Request for pipe : " ++ show pID ++ " on road : " ++ show r ++" ("++ show p ++ ")"
instance Show Research where show (Research rID ttl _ _) = "Research for : " ++ show rID ++ " ttl : " ++ show ttl
instance Show Answer where show (Answer c ttl r sID _ ) = "Answer for : " ++ show (_cResID c) ++ " from : " ++ show sID ++ " ttl : " ++ show ttl ++ " ROAD : " ++ show r


makeLenses ''Request
makeLenses ''Research
makeLenses ''Answer
makeLenses ''RessourceCert


instance SignedClass Request where scHash (Request _ r epk t pK pID _ c) = B.concat [ c , encode (r,epk,t,pK,pID) ]
                                   scSignature = view reqPipeSig
                                   scPushSignature r s = set reqPipeSig s r
instance IntroClass Request where icPubKey = view reqSourceKey

instance SignedClass Answer where scHash (Answer (RessourceCert k t t' rid _) _ _ sID _) = encode (k, t, t', rid, sID) 
                                  scSignature = view $ ansCert . cResSig
                                  scPushSignature a s = over ansCert (set cResSig s) a
instance IntroClass Answer where icPubKey = view $ ansCert . cResSourceKey 



instance Binary L2
instance Binary Request
instance Binary Research
instance Binary Answer
instance Binary RessourceCert


