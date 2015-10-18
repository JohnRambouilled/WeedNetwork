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


import Class
import Ed25519

--type EventSource a = (Event a, Handler a)

newtype KeyHash = KeyHash RawData deriving (Eq, Ord, Generic)
instance Binary KeyHash
type Payload = RawData

class SignedClass a where scHash :: a -> RawData
                          scKeyHash :: a -> KeyHash
                          scSignature :: a -> Signature
                          scPushSignature :: a -> Signature -> a

instance SignedClass a => IDable a KeyHash where extractID = scKeyHash

class SignedClass a => IntroClass a where icPubKey :: a -> PubKey

type CryptoMap e = EventEntryMap KeyHash e
type CryptoMapMod t e = ModEvent t (CryptoMap e)
type CryptoEntryEvent t e i = Event t (i, EventEntry e)

buildCryptoMap :: (Frameworks t, IntroClass i, SignedClass e) => Event t i -> Event t e -> Moment t (CryptoMapMod t e, CryptoEntryEvent t e i)
buildCryptoMap introE packetE = do modE <- newModEvent M.empty --creation de la Map
                                   newEntryE <- execute (insertCryptoKey (meModifier modE) <$> introE) 
                                   reactimate $ fst <$> newEntryE --On ajoute les nouvelles entrées
                                   reactimate . packetActions $ meLastValue modE    --On fire les packets dans la map
                                   pure (modE, snd <$> newEntryE)
    where insertCryptoKey :: (IntroClass i, SignedClass e) => Modifier (CryptoMap e) -> i -> FrameworksMoment (IO (), (i, EventEntry e))
          insertCryptoKey mod intro = FrameworksMoment $ do entry <- liftIO $ newEventEntry (checkSig $ icPubKey intro)
                                                            pure (mod $ insertEntry (scKeyHash intro) entry, (intro, entry))
          packetActions eM = filterJust $ apply (fireKey <$> eM) packetE 



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



