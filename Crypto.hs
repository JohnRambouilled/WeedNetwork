{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, FunctionalDependencies #-}
module Crypto (module Crypto,
               module Ed25519)
where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import GHC.Generics
import Reactive.Banana
import Reactive.Banana.Frameworks

import Timer
import Class
import Ed25519


newtype KeyHash = KeyHash RawData deriving (Eq, Ord, Generic)
type Payload = RawData

emptyPayload = Data.ByteString.Lazy.empty :: Payload

class SignedClass a where scHash :: a -> RawData
                          scKeyHash :: a -> KeyHash
                          scSignature :: a -> Signature
                          scPushSignature :: a -> Signature -> a

instance SignedClass a => IDable a KeyHash where extractID = scKeyHash

class SignedClass a => IntroClass a where icPubKey :: a -> PubKey

type CryptoMap t a = ModEvent t (M.Map KeyHash a)
type CryptoEvent t a i = Event t (Either (KeyHash, a) (i, a))


buildCryptoMap :: (Frameworks t, IntroClass i, SignedClass e, EventManager a e) 
                => Time -> (i -> IO a) -> Event t i -> Event t KeyHash -> Event t e -> Moment t (CryptoMap t a, CryptoEvent t a i)
buildCryptoMap t toEntry introE decoE packetE = do modE <- newModEvent M.empty --creation de la Map
                                                   ceE <- insertCryptoKey modE 
                                                   reactimate . packetActions $ meLastValue modE    --On fire les packets dans la map
                                                   pure (restrictModEvent (snd . snd <$>) (flip $ pure id) modE, makeCryptoE <$> ceE)
    where --insertCryptoKey :: (Frameworks t, IntroClass i, SignedClass e) => ModEvent t (CryptoMapTO e) -> Moment t (CryptoEntryEvent t e i)
          insertCryptoKey mod = do (ceE, ceH) <- newEvent
                                   reactimate $ onIntro ceH <$> filterE checkIntro introE
                                   timeOE <- insertTOEvent t mod $ insertI <$> ceE
                                   deleteTOReactimate mod decoE
                                   pure $ union (Left <$> timeOE) (Right <$> ceE)
                                where onIntro h intro = do entry <- toEntry intro
                                                           h $ (intro, (checkSig $ icPubKey intro, entry))
                                      insertI (i, e) = (scKeyHash i, e)
          packetActions eM = filterJust $ apply (fireKeyWith (fireEntry . snd) <$> eM) packetE 
          fireEntry (sigCheck, a) e = if sigCheck e then emFire a $ e else pure ()
          checkIntro i = checkSig (icPubKey i) i
          makeCryptoE (Right (i, (_,a))) = Right (i,a)
          makeCryptoE (Left (i, (_,a))) = Left (i,a)


checkSig :: SignedClass a => PubKey -> a -> Bool
checkSig k a  = checkSignature k (scSignature a) $ scHash a

sign :: SignedClass a => KeyPair-> a -> a
sign (pK, k) a = scPushSignature a $ makeSignature k pK $ scHash a


computeHashFromKey :: PubKey -> KeyHash
computeHashFromKey = KeyHash . computeHash



isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft

fromLeft (Left x) = x
fromLeft _ = error $ "fromLeft on Right"

fromRight (Right x) = x
fromRight _ = error $ "fromRight on Left"

instance Show KeyHash where show (KeyHash d) = prettyPrint d
instance Binary KeyHash

