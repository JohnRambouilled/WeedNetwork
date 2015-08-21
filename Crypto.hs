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
import FRP.Sodium.Internal hiding (Event)

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

dataSignedPayload = signedPayload . dataContent

checkSig _ _ _ = True
sign _ _ = 0
decrypt _ = id
computeHashFromKey _ = 0



{-| Associe à chaque keyhash le flux de paquets vérifiés signés par cette clef |-}
type CryptoMap = M.Map KeyHash (Event DataPacket)

data CryptoOrder = CryptoAdd KeyHash (Event DataPacket)
                 | CryptoAdd' KeyHash PubKey
                 | CryptoDelete KeyHash


{-| Seule fonction à utiliser. Retourne la map des events de datapacket signés par chaque clef
    ainsi qu'une fonction pour envoyer des commandes. |-}
runCryptoModule :: Event Introduce -> Event DataPacket -> Reactive (Behaviour CryptoMap , CryptoOrder -> Reactive ())
runCryptoModule introE dataE = do (ordersE,fireOrder) <- newEvent
                                  cMapB <- accum M.empty $ onOrder dataE <$> ordersE
                                  listenTrans (handleCryptoModule introE dataE fireOrder) id
                                  pure (cMapB,fireOrder)




onOrder :: Event DataPacket -> CryptoOrder -> CryptoMap -> CryptoMap
onOrder _ (CryptoAdd kH dataE) = M.insert kH dataE
onOrder dataE (CryptoAdd' kH pKey) = M.insert kH (filterDataStream' pKey dataE)
onOrder _ (CryptoDelete kH) = M.delete kH



checkIntroSig :: Introduce -> Bool
checkIntroSig (Introduce kH pKey sig pay) = computeHashFromKey pKey == kH &&
                                            checkSig pKey sig (signedPayload pay)



handleCryptoModule :: Event Introduce 
                   -> Event DataPacket 
                   -> Handler CryptoOrder
                   -> Event (Reactive ())
handleCryptoModule introE dataE fireOrder = filterJust $ onIntro <$> introE
    where onIntro :: Introduce -> Maybe (Reactive ())
          onIntro intro@(Introduce kH pKey _ _)
                | checkIntroSig intro = Just $ fireOrder $ CryptoAdd kH (filterE (filterDataStream pKey) dataE) 
                | otherwise = Nothing  

filterDataStream :: PubKey -> DataPacket -> Bool
filterDataStream pKey (DataPacket kH sig pl) = kH == computeHashFromKey pKey &&
                                         checkSig pKey sig (signedPayload pl)


filterDataStream' pKey = filterE (filterDataStream pKey)
                                

filterDecode :: (Binary a) => Event RawData -> Event a
filterDecode rawE = extractData <$> filterE isRight (decodeOrFail <$> rawE)
  where extractData (Right (_,_,r)) = r

isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft


swapB :: (Ord k, Eq k) => Behaviour (Reactive (M.Map k a)) -> Reactive (Behaviour (M.Map k a))
swapB mapB = hold M.empty $ execute $ value mapB

