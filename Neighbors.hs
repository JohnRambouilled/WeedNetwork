{-# LANGUAGE DeriveGeneric #-}
module Neighbors where

import Crypto
import Routing
import Class
import Ressource

import Data.ByteString hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import FRP.Sodium 
import FRP.Sodium.Internal hiding (Event)
import GHC.Generics



data NeighIntro = NeighIntro {neighIKeyID :: KeyHash, neighIPubKey :: PubKey,  neighISig :: Signature, neighIPayload :: Payload} 
data NeighData  = NeighData  {neighDKeyID :: KeyHash, neighDSig :: Signature, neighDContent :: NeighDataContent} |
                  NeighClose {neighDKeyID :: KeyHash, neighDSig :: Signature, neighCPayload :: Payload}

data NeighDataContent = NeighReq Request | NeighRes Research | NeighAns Answer deriving Generic
instance Binary NeighDataContent


instance SignedClass NeighIntro where scHash (NeighIntro kH pK _ pay) = encode (kH, pK, pay)
                                      scKeyHash = neighIKeyID
                                      scSignature = neighISig
                                      scPushSignature i s = i{neighISig = s}
instance IntroClass NeighIntro where icPubKey = neighIPubKey

instance SignedClass NeighData  where scHash (NeighData  kH _ pay) = encode (kH, pay)
                                      scKeyHash = neighDKeyID
                                      scSignature = neighDSig
                                      scPushSignature d s = d{neighDSig = s}

type NeighMap = EventEntryMap KeyHash NeighData
type NeighMapBhv = BhvTpl NeighMap 

data Neighborhood = Neighborhood {nbhNeighMap :: NeighMapBhv,
                                  nbhRequests :: Event Request,
                                  nbhResearchs :: Event Research,
                                  nbhAnswers :: Event Answer}

buildNeighborhood :: Event NeighIntro -> Event NeighData -> Reactive Neighborhood
buildNeighborhood introE dataE = do 
                                    ((nM, nMH), _) <- buildCryptoMap introE dataE
                                    (reqE, reqH) <- newEvent'
                                    (resE, resH) <- newEvent'
                                    (ansE, ansH) <- newEvent'
                                    listenTrans (allEvents nM) $ onDataEvent (Handler $ nMH . M.delete) reqH resH ansH
                                    pure $ Neighborhood (nM,nMH) reqE resE ansE


onDataEvent :: Handler KeyHash -> Handler Request -> Handler Research -> Handler Answer -> NeighData -> Reactive ()
onDataEvent h _ _ _ (NeighClose kID _ _) = fire h $ kID
onDataEvent _ reqH _ _ (NeighData _ _ (NeighReq r)) = fire reqH $ r
onDataEvent _ _ resH _ (NeighData _ _ (NeighRes r)) = fire resH $ r 
onDataEvent _ _ _ ansH (NeighData _ _ (NeighAns a)) = fire ansH $ a



