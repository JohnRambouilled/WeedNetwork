module Crypto where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
--import Reactive.Banana.Combinators
--import Reactive.Banana.Frameworks
--import Reactive.Banana.Switch
--import Control.Event.Handler
import FRP.Sodium

type Handler a = a -> Reactive ()
type EventSource a = (Event a, Handler a)

type KeyHash = Int
type PubKey = Int
type PrivKey = Int
type Signature = Int
type RawData = ByteString

data Payload =  Payload { signedPayload :: RawData,
                          unsignedPayload :: RawData}


type CryptoPacket = Either Introduce DataPacket


data Introduce = Introduce   {introKeyID :: KeyHash, introKey :: PubKey,  introSig :: Signature, introContent :: Payload} 
data DataPacket = DataPacket  {datakeyID :: KeyHash, dataSig :: Signature, dataContent :: Payload}

checkSig _ _ _ = True
sign _ _ = 0
decrypt _ = id
computeHashFromKey _ = 0


data CryptoEntry = CryptoEntry {pubKey :: PubKey,
                                handlePacket :: Handler DataPacket}

data CryptoNewKey = CryptoNewKey {cnkPayload :: Introduce,
                                  cnkKeyID :: KeyHash, 
                                  cnkCallback :: Event DataPacket}


type CryptoMap = M.Map KeyHash CryptoEntry

data CryptoOrders = CryptoAdd KeyHash CryptoEntry |
                    CryptoDelete KeyHash


checkIntroSig (Introduce kH pKey sig pay) = computeHashFromKey pKey == kH &&
                                            checkSig pKey sig (signedPayload pay)

                                


{-| Calls outH everytimes a new introduce has been received from an unknown key.
 -  The cryptoEntry contains a handler called for each new datapacket from this source. |-}
buildCrypto :: EventSource CryptoOrders -> Handler CryptoNewKey -> Event CryptoPacket -> Reactive ()
buildCrypto (orderE, fireOrder) outH inE = let (introE, dataE) = split inE
                                           in do cryptoMap <- genCryptoMap orderE
                                                 let (newIntroE, _) = filterNewIntro (filterE checkIntroSig introE) cryptoMap
                                                 listen newIntroE $ onNewIntro outH fireOrder
                                                 listen (snapshot onDataPacket cryptoMap dataE) id
                                           
                              

onDataPacket :: CryptoMap -> DataPacket -> IO ()
onDataPacket cM d@(DataPacket kH s pay) = case M.lookup kH cM of
                        Nothing -> pure ()
                        Just (CryptoEntry pK h) -> when (checkSig pK s (signedPayload pay)) $ h d



genCryptoMap :: Event CryptoOrders -> Reactive (Behavior CryptoMap)
genCryptoMap orders = accum M.empty (onOrder <$> orders)

onOrder :: CryptoOrders -> CryptoMap -> CryptoMap
onOrder (CryptoAdd kH cE) = M.insert kH cE
onOrder (CryptoDelete kH) = M.delete kH

filterNewIntro :: Event Introduce -> Behavior CryptoMap -> (Event Introduce, Event Introduce)
filterNewIntro introE cMapB = split $ snapshot makeEither cMapB introE
    where makeEither :: CryptoMap -> Introduce -> Either Introduce Introduce
          makeEither cM i = case M.lookup (introKeyID i) cM of
                                Nothing -> Left i
                                Just _ -> Right i

onNewIntro :: Handler CryptoNewKey -> Handler CryptoOrders -> Introduce -> Reactive ()
onNewIntro outH ordH i@(Introduce kH k _ _) = do (addE, fire) <- newEvent
                                                 ordH $ CryptoAdd kH (CryptoEntry k fire)
                                                 outH $ CryptoNewKey i kH addE


