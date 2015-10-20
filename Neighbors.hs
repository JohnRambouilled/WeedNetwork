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
import Reactive.Banana
import Reactive.Banana.Frameworks
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
type NeighMapBhv t = ModEvent t NeighMap 

data Neighborhood t = Neighborhood {nbhNeighMap :: NeighMapBhv t,
                                    nbhRequests :: Event t Request,
                                    nbhResearchs :: Event t Research,
                                    nbhAnswers :: Event t Answer}

buildNeighborhood :: Frameworks t => Event t NeighIntro -> Event t NeighData -> Moment t (Neighborhood t)
buildNeighborhood introE dataE = do 
                                    (nMod, _) <- buildCryptoMap introE dataE
                                    (reqE, reqH) <- newEvent
                                    (resE, resH) <- newEvent
                                    (ansE, ansH) <- newEvent
                                    allEvents <- mergeEvents $ meChanges nMod
                                    reactimate $ onDataEvent (meModifier nMod . M.delete) reqH resH ansH <$> allEvents
                                    pure $ Neighborhood nMod reqE resE ansE



onDataEvent :: Handler KeyHash -> Handler Request -> Handler Research -> Handler Answer -> NeighData -> IO ()
onDataEvent h _ _ _ (NeighClose kID _ _) = h  kID
onDataEvent _ reqH _ _ (NeighData _ _ (NeighReq r)) = reqH  r
onDataEvent _ _ resH _ (NeighData _ _ (NeighRes r)) = resH  r 
onDataEvent _ _ _ ansH (NeighData _ _ (NeighAns a)) = ansH  a

showNeighborhood :: Neighborhood t -> Event t String
showNeighborhood = showMap "Neighborhood" . nbhNeighMap

