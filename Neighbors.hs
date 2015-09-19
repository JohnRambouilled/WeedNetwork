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


type Payload = RawData

data NeighIntro = NeighIntro {introKeyID :: KeyHash, introKey :: PubKey,  introSig :: Signature, introContent :: Payload} 
data NeighData  = NeighData  {dataKeyID :: KeyHash, dataSig :: Signature, dataContent :: NeighDataContent}

data NeighDataContent = NeighDeco | NeighReq Request | NeighResearch | NeighAnswer deriving Generic
instance Binary NeighDataContent



instance SignedClass NeighIntro where scHash (NeighIntro kH pK _ pay) = encode (kH, pK, pay)
                                      scKeyHash = introKeyID
                                      scSignature = introSig
instance IntroClass NeighIntro where icPubKey = introKey

instance SignedClass NeighData  where scHash (NeighData  kH _ pay) = encode (kH, pay)
                                      scKeyHash = dataKeyID
                                      scSignature = dataSig

type NeighMapBhv = Behavior (EventMap KeyHash NeighData)

data NeighPacketEvents = NeighPacketEvents {
                                            npeRequest :: Event Request,
                                            npeDecoHandle :: Handler KeyHash}

buildNeighPacketEvents :: Event NeighIntro -> Event NeighData -> Reactive NeighPacketEvents
buildNeighPacketEvents introE dataE = do (decoE, decoH) <- newEvent
                                         nM <- buildCryptoMap introE decoE dataE
                                         (reqE, reqH) <- newEvent
                                         listenTrans (switchE (allEvents' <$> nM)) $ onDataEvent decoH reqH
                                         pure $ NeighPacketEvents reqE decoH


onDataEvent :: Handler KeyHash -> Handler Request -> NeighData -> Reactive ()
onDataEvent h _ (NeighData kID _ NeighDeco) = h kID
onDataEvent _ h (NeighData _ _ (NeighReq r)) = h r
onDataEvent _ _ _ = pure ()


{-

data NeighDeco  = NeighDeco  {decoKeyID :: KeyHash, decoSig :: Signature, decoContent :: Payload}

instance Signed NeighDeco  where hash (NeighDeco  kH _ pay)    = encode (kH, pay)
                                 keyHash = decoKeyID
                                 sig = decoSig

type NeighPacketEvents = ( Event NeighIntro , Event NeighData , Event NeighDeco )  

type NeighMap = CryptoMap NeighData

{- | Main function : manage connections and deconnections from neighbors, and return the flow of correctly signed data packet.-}
buildNeighbors :: NeighPacketEvents -> Reactive (Event NeighData)
buildNeighbors (introE, dataE, decoE) = do nMap <- buildNeighMap introE decoE
                                           return . filterJust $ snapshot onData dataE nMap
        where onData :: NeighData -> NeighMap -> Maybe NeighData
              onData nD@(NeighData kID s _) nMap = M.lookup kID nMap >>= checkDataSig nD s
              checkDataSig nD s (pK,h) = if checkSig pK s nD then Nothing else Just nD

buildNeighMap :: Event NeighIntro -> Event NeighDeco -> Reactive (Behaviour NeighMap)
buildNeighMap introE decoE = accum M.empty $ merge (onIntro <$> introE) (onDeco <$> decoE) 
    where onIntro :: NeighIntro -> NeighMap -> NeighMap
          onIntro nI@(NeighIntro kID pK s _) = if checkSig pK s nI then M.insert kID pK else id
          onDeco :: NeighDeco -> NeighMap -> NeighMap
          onDeco nD@(NeighDeco kID s _) = M.update (\pK -> if checkSig pK s nD then Nothing else Just pK) kID


buildNeighbors :: Handler Payload -> Handler CryptoOrders -> Handler CryptoNewKey
buildNeighbors payH cOrderH (CryptoNewKey i kH addH) = case decodeOrFail (signedPayload $ introContent i) of
                                                                Right (_,_, NeighHello d) -> void $ register addH $ manageData
                                                                Left (_,_,e) -> print e
    where manageData (DataPacket _ _ pay) = case decodeOrFail $ signedPayload pay of
                                        Left (_,_,e) -> print e
                                        Right (_,_,NeighClose d) -> cOrderH $ CryptoDelete kH
                                        Right (_,_,NeighData d)  -> payH pay

instance Binary NeighHello
instance Binary NeighData-}
