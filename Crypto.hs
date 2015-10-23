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

--type EventSource a = (Event a, Handler a)

newtype KeyHash = KeyHash RawData deriving (Eq, Ord, Generic, Show)
instance Binary KeyHash
type Payload = RawData

emptyPayload = Data.ByteString.Lazy.empty :: Payload

class SignedClass a where scHash :: a -> RawData
                          scKeyHash :: a -> KeyHash
                          scSignature :: a -> Signature
                          scPushSignature :: a -> Signature -> a

instance SignedClass a => IDable a KeyHash where extractID = scKeyHash

class SignedClass a => IntroClass a where icPubKey :: a -> PubKey

type CryptoMapTO e = TimeMap KeyHash (EventEntry e)
type CryptoMap e = EventEntryMap KeyHash e
type CryptoMapMod t e = ModEvent t (CryptoMap e)
type CryptoMapModTO t e = ModEvent t (CryptoMapTO e)
type CryptoEvent t e i = Event t (Either (KeyHash, EventEntry e) (i, EventEntry e))



buildCryptoMapTO :: (Frameworks t, IntroClass i, SignedClass e) => Time -> Event t i -> Event t e -> Moment t (CryptoMapModTO t e, CryptoEvent t e i)
buildCryptoMapTO t introE packetE = do modE <- newModEvent M.empty --creation de la Map
                                       ceE <- insertCryptoKey modE 
                                       reactimate . packetActions $ meLastValue modE    --On fire les packets dans la map
                                       pure (modE, ceE)
    where --insertCryptoKey :: (Frameworks t, IntroClass i, SignedClass e) => ModEvent t (CryptoMapTO e) -> Moment t (CryptoEntryEvent t e i)
          insertCryptoKey mod = do (ceE, ceH) <- newEvent
                                   reactimate $ onIntro ceH <$> introE
                                   decoE <- insertTOEvent t mod $ insertI <$> ceE
                                   pure $ union (Left <$> decoE) (Right <$> ceE)
                                where onIntro h intro = do entry <- liftIO $ newEventEntry (checkSig $ icPubKey intro)
                                                           h $ (intro, entry)
                                      insertI (i, e) = (scKeyHash i, e)
          packetActions eM = filterJust $ apply (fireKeyWith snd <$> eM) packetE 



buildCryptoMap :: (Frameworks t, IntroClass i, SignedClass e) => Event t i -> Event t e -> Moment t (CryptoMapMod t e, CryptoEvent t e i)
buildCryptoMap introE packetE = do modE <- newModEvent M.empty --creation de la Map
                                   newEntryE <- execute (insertCryptoKey (meModifier modE) <$> introE) 
                                   reactimate $ fst <$> newEntryE --On ajoute les nouvelles entrées
                                   reactimate . packetActions $ meLastValue modE    --On fire les packets dans la map
                                   pure (modE, Right . snd <$> newEntryE)
    where insertCryptoKey :: (IntroClass i, SignedClass e) => Modifier (CryptoMap e) -> i -> FrameworksMoment (IO (), (i, EventEntry e))
          insertCryptoKey mod intro = FrameworksMoment $ do entry <- liftIO $ newEventEntry (checkSig $ icPubKey intro)
                                                            pure (mod $ insertEntry (scKeyHash intro) entry, (intro, entry))
          packetActions eM = filterJust $ apply (fireKey  <$> eM) packetE 



checkSig :: SignedClass a => PubKey -> a -> Bool
checkSig k a  = checkSignature k (scSignature a) $ scHash a

sign :: SignedClass a => KeyPair-> a -> a
sign (pK, k) a = scPushSignature a $ makeSignature k pK $ scHash a

decryptPrivKey :: DHPubKey -> PrivKey -> Maybe KeyPair
decryptPrivKey dh k = privKeyToDHPrivKey k >>= exctractKey dh

computeHashFromKey :: PubKey -> KeyHash
computeHashFromKey = KeyHash . computeHash



isLeft (Left _) = True
isLeft _ = False
isRight = not . isLeft

fromLeft (Left x) = x
fromLeft _ = error $ "fromLeft on Right"

fromRight (Right x) = x
fromRight _ = error $ "fromRight on Left"



