{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FlexibleInstances, FunctionalDependencies #-}
module Crypto (module Crypto,
               module Ed25519)
where
import Data.ByteString.Lazy hiding (split)
import Data.Binary
import Control.Monad
import Control.Lens
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

{- | Classe de type des packets signés -}
class SignedClass a where scHash :: a -> RawData    -- ^ Hash du packet (utilisé pour signer, et vérifier les signatures)
                          scKeyHash :: a -> KeyHash     -- ^ KeyHash de la clef publique utilisée pour signer le packet
                          scSignature :: a -> Signature  -- ^ Signature du packet
                          scPushSignature :: a -> Signature -> a   -- ^ fonction de remplacement de la signature (permet de signer)

instance SignedClass a => IDable a KeyHash where extractID = scKeyHash  -- ^ Instance de IDable des packets signés (identifiés par le KeyHash)


{- | Classe de type des packets introduisant une clef : il s'agit de packets signés, contenant de plus une clef publique -}
class SignedClass a => IntroClass a where icPubKey :: a -> PubKey


type CryptoMap e = BehaviorC (M.Map KeyHash (Channel e))  -- ^ map des events de packets signés, triés par keyHash
type CryptoEvent i e = Event (Either i (i, Channel e))    -- ^ Event de nouvelles connexions Left pour les clef déja connues, et Right pour les nouvelles clef.

{- | Construction de la cryptoMap a partir d'un Event d'introduce, et d'un Event de packets. 
 -   Les entrées sont supprimées lorsque le Channel est fermé.
 -   Les signatures des packets sont vérifiées. Renvoi la Map des events, l'Event de nouvelles connexions   -}
buildCryptoMap :: (IntroClass i, SignedClass e) => Event i -> Event e -> MomentIO (CryptoMap e, CryptoEvent i e)
buildCryptoMap introE packetE = do (buildE, buildH) <- newEvent
                                   cMap <- buildCloseMapWith buildE
                                   cryptoE <- execute $ apply (onIntro <$> bmLastValue cMap) $ filterE checkIntro introE
                                   reactimate $ buildH . buildMap <$> snd (split cryptoE)
                                   fireKeyBhv (fmap snd <$> bmLastValue cMap) packetE
                                   pure ((fst <$>) <$> bmBhvC cMap, fmap (over _2 fst) <$> cryptoE)
    where makeChannel :: (IntroClass i, SignedClass e) => i -> MomentIO (ChannelEntry e)
          makeChannel i = do (ch, h) <- newChannel
                             let ch' = ch{chanEvent = filterSig i $ chanEvent ch}
                             pure (ch', h)
          filterSig i = filterE (checkSig (icPubKey i))
          onIntro :: (IntroClass i, SignedClass e) => M.Map KeyHash (ChannelEntry e) -> i -> MomentIO (Either i (i, ChannelEntry e))
          onIntro m i = case scKeyHash i `M.lookup` m of 
                                  Just _ -> pure $ Left i
                                  Nothing -> do ee <- makeChannel i
                                                pure $ Right (i, ee)
          buildMap :: (IntroClass i, SignedClass e) => (i, ChannelEntry e) -> ((KeyHash, Channel e), ChannelEntry e)
          buildMap (i,e) = ((scKeyHash i, fst e),e)
          checkIntro i = checkSig (icPubKey i) i && computeHashFromKey (icPubKey i) == scKeyHash i 


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

