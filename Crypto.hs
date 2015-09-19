{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
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
import Class
import FRP.Sodium.Internal hiding (Event)

type EventSource a = (Event a, Handler a)

data KeyHash = KeyHash Int deriving (Eq, Ord)
type PubKey = Int
type PrivKey = Int
type Signature = Int
type RawData = ByteString

class Signed a where hash :: a -> RawData
                     keyHash :: a -> KeyHash
                     signature :: a -> Signature

instance Signed a => IDable a KeyHash where extractID = keyHash

class Signed a => IntroClass a where introKey :: a -> PubKey


buildCryptoMap :: (IntroClass i, Signed e) => Event i -> Event KeyHash -> Event e -> Reactive (Behavior (EventMap KeyHash e))
buildCryptoMap introE decoE packetE = do eM <- accum (EventMap M.empty) $ merge (deleteKey <$> decoE) (execute (insertCryptoKey <$> introE))
                                         listenTrans (packetActions eM) id
                                         pure eM
    where insertCryptoKey intro = do (event, fire) <- newEvent
                                     return $ insertEntry (keyHash intro) $ EventMapEntry fire event (checkSig $ introKey intro)
          packetActions eM = filterJust $ snapshot (flip fireKey) packetE eM



checkSig :: Signed a => PubKey -> a -> Bool
checkSig _ _  = True
sign :: Signed a => PrivKey -> a -> Signature
sign _ _ = 0
decrypt _ = id
computeHashFromKey _ = 0




--type HandlerMapBhv k a e = Behavior (M.Map k (a, Handler e))

{-
buildHandlerMapBhv :: Ord k => Event (k, a) -> Event k -> Event (k,e) -> Reactive (HandlerMapBhv k a e, Event ((k, a), Event e))
buildHandlerMapBhv introE decoE packetE = do mapBhv <- accum M.empty $ merge (snd <$> newE) (onDeco <$> decoE)
                                             listen (execute $ snapshot onPacket packetE mapBhv) $ \_ -> pure ()
                                             pure (mapBhv, fst <$> newE)
    where onIntro (k, a) = do (event, fire) <- newEvent
                              return $ ( ((k, a), event), M.insert k (a, fire))
          onDeco k = M.delete k
          onPacket (k,e) map = case M.lookup k map of Nothing -> pure ()
                                                      Just (a,h) -> h e
          newE = execute (onIntro <$> introE) 

type CryptoMap e = HandlerMapBhv KeyHash PubKey e

buildCryptoMap :: (Signed d, IntroClass i, Signed o) => Event i -> Event o -> Event d -> Reactive (CryptoMap d, Event ((KeyHash, PubKey), Event d) )
buildCryptoMap introE decoE dataE = do (decoKeyE, fireDecoKey) <- newEvent 
                                       buildHandlerMapBhv (filter checkIntro introE) decoKeyE 
                                       
-}
{-
buildCryptoMap :: (Signed d, IntroClass i, Signed o) => Event (i, Handler d) -> Event o -> Reactive (Behavior (CryptoMap d))
buildCryptoMap introE decoE = accum M.empty $ merge (onIntro <$> introE) (onDeco <$> decoE) 
    where onIntro (i,h) = if checkSig key (sig i) i then M.insert (keyHash i) (key, makeHandle key h) else id
                    where key = introKey i
          onDeco d = M.update (\(pK,h) -> if checkSig pK (sig d) d then Nothing else Just (pK,h)) $ keyHash d
          makeHandle k h d = if checkSig k (sig d) d then h d else pure ()
-}





--data Payload =  Payload { signedPayload :: RawData,
 --                         unsignedPayload :: RawData}


--type CryptoPacket = Either Introduce DataPacket


--data Introduce = Introduce   {introKeyID :: KeyHash, introKey :: PubKey,  introSig :: Signature, introContent :: Payload} 
--data DataPacket = DataPacket  {datakeyID :: KeyHash, dataSig :: Signature, dataContent :: Payload}

--dataSignedPayload = signedPayload . dataContent

{-| Associe à chaque keyhash le flux de paquets vérifiés signés par cette clef |
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
-}
isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft


swapB :: (Ord k, Eq k) => Behaviour (Reactive (M.Map k a)) -> Reactive (Behaviour (M.Map k a))
swapB mapB = hold M.empty $ execute $ value mapB

