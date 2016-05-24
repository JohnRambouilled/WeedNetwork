{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, FunctionalDependencies #-}
module Types.Crypto (module Types.Ed25519,
                     module Types.Crypto)
where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import GHC.Generics

import Types.Ed25519


newtype KeyHash = KeyHash RawData deriving (Eq, Ord, Generic)
type Payload = RawData

emptyPayload = Data.ByteString.Lazy.empty :: Payload

{- | Classe de type des packets signés -}
class SignedClass a where scHash :: a -> RawData    -- ^ Hash du packet (utilisé pour signer, et vérifier les signatures)
                          scKeyHash :: a -> KeyHash     -- ^ KeyHash de la clef publique utilisée pour signer le packet
                          scSignature :: a -> Signature  -- ^ Signature du packet
                          scPushSignature :: a -> Signature -> a   -- ^ fonction de remplacement de la signature (permet de signer)

--instance SignedClass a => IDable a KeyHash where extractID = scKeyHash  -- ^ Instance de IDable des packets signés (identifiés par le KeyHash)


{- | Classe de type des packets introduisant une clef : il s'agit de packets signés, contenant de plus une clef publique -}
class SignedClass a => IntroClass a where icPubKey :: a -> PubKey



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

