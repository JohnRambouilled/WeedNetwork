{-# LANGUAGE DeriveGeneric #-}
module Neighbors where

import Crypto
import Routing
import Class

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

data NeighDataContent = NeighReq Request | NeighResearch | NeighAnswer deriving Generic
instance Binary NeighDataContent


instance SignedClass NeighIntro where scHash (NeighIntro kH pK _ pay) = encode (kH, pK, pay)
                                      scKeyHash = neighIKeyID
                                      scSignature = neighISig
instance IntroClass NeighIntro where icPubKey = neighIPubKey

instance SignedClass NeighData  where scHash (NeighData  kH _ pay) = encode (kH, pay)
                                      scKeyHash = neighDKeyID
                                      scSignature = neighDSig

type NeighMap = EventEntryMap KeyHash NeighData
type NeighMapBhv = Behavior NeighMap 

data Neighborhood = Neighborhood {nbhNeighMap :: NeighMapBhv,
                                  nbhRequests :: Event Request,
                                  nbhModNeighMap :: Modifier NeighMap}

buildNeighborhood :: Event NeighIntro -> Event NeighData -> Reactive Neighborhood
buildNeighborhood introE dataE = do 
                                    ((nM, nMH), _) <- buildCryptoMap introE dataE
                                    (reqE, reqH) <- newEvent'
                                    listenTrans (allEvents nM) $ onDataEvent (Handler $ nMH . M.delete) reqH
                                    pure $ Neighborhood nM reqE nMH


onDataEvent :: Handler KeyHash -> Handler Request -> NeighData -> Reactive ()
onDataEvent h _ (NeighClose kID _ _) = fire h $ kID
onDataEvent _ reqH (NeighData _ _ (NeighReq r)) = fire reqH $ r
