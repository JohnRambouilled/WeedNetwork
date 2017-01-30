{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell   #-}
module Packets.Layer2 where

import Types.Crypto
import Types.Packets


import Data.Binary
import Control.Monad
import Control.Lens
import qualified Data.Map as M
import GHC.Generics

data L2 = L2Request Request | L2Research Research | L2Answer Answer 
    deriving (Generic, Show)


data Request = Request {_reqPosition :: Number, -- ^ Position on the road, changed during routing (NOT SIGNED)
                        _reqLength :: Number, -- ^ Total length of the road
                        _reqRoad :: Road,  -- ^ Road : list of UserID
                        _reqSourceKey :: PubKey,  -- ^ DHPubKey of the origin of the request
                        _reqTime :: Time,    -- ^ Send time of the request
                        _reqPipeKey :: PipePubKey, -- ^ Public Key of the opening pipe
                        _reqPipeID  :: PipeID,  -- ^ PipeID of the pipe (KeyHash of the Road)
                        _reqPipeSig :: Signature,  -- ^ Signature of the packet's Hash
                        _reqContent :: RawData}   -- ^ extra content if needed (cause why not)
    deriving Generic


data Research = Research {_resID :: RessourceID,
                          _resTTL :: TTL,
                          _resRoad :: Road,
                          _resCnt :: RawData}
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

instance Show Request where show (Request p l r _ t _ pID _ _) = "Request for pipe : " ++ show pID ++ " on road : " ++ show r ++" ("++ show p ++", "++ show l ++")"
instance Show Research where show (Research rID ttl _ _) = "Research for : " ++ show rID ++ " ttl : " ++ show ttl
instance Show Answer where show (Answer c ttl r sID _ ) = "Answer for : " ++ show (_cResID c) ++ " from : " ++ show sID ++ " ttl : " ++ show ttl ++ " ROAD : " ++ show r


makeLenses ''Request
makeLenses ''Research
makeLenses ''Answer
makeLenses ''RessourceCert


instance SignedClass Request where scHash (Request n l r epk t pK pID s c) = encode (l,r,epk,t,pK,pID,c)
                                   scSignature = view reqPipeSig
                                   scPushSignature r s = set reqPipeSig s r
instance IntroClass Request where icPubKey = view reqSourceKey

instance SignedClass Answer where scHash (Answer (RessourceCert k t t' rid _) _ _ sID r) = encode (k, t, t', rid, sID, r)
                                  scSignature = view $ ansCert . cResSig
                                  scPushSignature a s = over ansCert (set cResSig s) a
instance IntroClass Answer where icPubKey = view $ ansCert . cResSourceKey 



instance Binary L2
instance Binary Request
instance Binary Research
instance Binary Answer
instance Binary RessourceCert


