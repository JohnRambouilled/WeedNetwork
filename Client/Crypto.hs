{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ExistentialQuantification #-}
module Client.Crypto
     (module Client.Crypto.Module,
      module Client.Crypto.Crypto,
      module Client.Crypto)
 where
import Control.Monad.State
import Data.List 
import Data.Maybe
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.RWS.Lazy

import Client.Class
import Client.Packet
import Log
import Timer
import Client.Crypto.Module
import Client.Crypto.Crypto


{- Main function for generating Callbacks on IntroducePacket from unknown keys. Arguments are :
        * MVar Crypto and Timer
        * TimeOut of the entry to be register fo the key
        * MVar containing the module to be called
        * Hash function to be applied to the packet (returning the entry type of the callback module)
        * Function to be applied to the result, returning : * Maybe a list of callbacks to register for the introduced key
                                                            * A list of Packets to relay
                                                            * An action to be called when the entry is deleted

  The time-Out is refreshed when an introducePacket is received from the registered key, then the callbacks are called.
-}
--genCryptoCallback :: (Modules a p r) => MVar Crypto -> MVar Timer -> DiffTime -> MVar a -> HashFunction p -> (Packet -> r -> CryptoT IO (Maybe [CryptoAction], [Packet],IO ())) -> CryptoCB Packet Packet
genCryptoCallback :: (Modules a p r) => MVar Crypto -> MVar Timer -> DiffTime -> MVar a -> HashFunction p 
                                    -> (r -> CryptoCB Packet (Maybe [CryptoAction], [Packet], IOLog () ) ) 
                                    -> CryptoCB Packet [Packet]
genCryptoCallback crypto timer ttl aV hF oF  = ask >>= genCallback aV <$> inFun <*> outFun 
        where --inFun :: (Modules a p r) => Packet -> Behaviour Crypto Packet IO p
              inFun p = if checkIntroduce p then do ans <- maybeToList <$> checkHashFunction (key p) hF
                                                    keepLog CryptoLog Normal $ if null ans then "Fail hashFunction" else "calling cryptoCallback"
                                                    pure ans
                                          else (keepLog CryptoLog Normal "not a correct introduce") >> pure [] 

              outFun p rs = concat <$> mapM (\ri -> do (mDCBs ,pL,onTimeOut) <- oF ri
                                                       outFun' p mDCBs pL onTimeOut) rs 
              outFun' p mDCBs pL onTimeOut = do 
                              case mDCBs of
                                Nothing -> (keepLog CryptoLog Normal "No callbacks to register") >> return pL
                                Just dCBL -> do (refresh, free) <- liftIO $ registerTimerM timer ttl ((liftIO $ runRWSTMVar crypto () $ unregisterKeyEntry ( keyID p)) >> return False)
                                                let dCBL' = (\_  ->  do pkt <- ask 
                                                                        when (isIntroduce pkt) $ liftLog refresh 
                                                                        pure []):dCBL
                                                keepLog CryptoLog Important $  "crypto : register callback for key :" ++ show (keyID p)
                                                registerDataCallback (keyID p) (key p) dCBL' (onTimeOut >> liftLog free) >> return pL
              checkIntroduce p = if isIntroduce p then pubKeyToHash (key p) == (runKeyHash $ keyID p) else False


-- | genCryptoCallback where the callback-module's MVar is transmitted to the output-Function.
--genMCryptoCallback :: Modules a p r => MVar Crypto -> MVar Timer -> DiffTime -> MVar a -> HashFunction p -> (MVar a -> Packet -> r -> CryptoT IO (Maybe [CryptoAction], [Packet],IO ())) -> CryptoCB Packet Packet
genMCryptoCallback :: (Modules a p r) => MVar Crypto -> MVar Timer -> DiffTime -> MVar a -> HashFunction p 
                                     -> (MVar a -> r -> CryptoCB Packet (Maybe [CryptoAction], [Packet], IOLog () ) ) 
                                     -> CryptoCB Packet [Packet]
genMCryptoCallback crypto timer ttl aV hF oFV = genCryptoCallback crypto timer ttl aV hF (oFV aV)

-- | Transform a DataCB in cryptoAction : check the signature of the packet with checkHashFunction and call the callback with the returned value.
runDataCB :: DataCB -> CryptoAction
runDataCB (DataCB hF clbk) (KeyEntry k _ _) = do 
        qM <- checkHashFunction k hF
        case qM of
              Nothing -> pure []
              Just q -> withRWST (\_ s -> (q,s)) $ clbk 
        


