{-# LANGUAGE DeriveGeneric #-}

module Pipes where

import Crypto hiding (onOrder)
import Routing hiding (onOrder)
import FRP.Sodium
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M



type PipeMap = M.Map PipeID (Event PipeMessage)
data PipeManagerEntry = PipeManagerEntry {pmePipeMap :: Behaviour PipeMap,
                                          pmePipeFired :: Event (Reactive ()),
                                          pmeFire :: Handler PipeOrder}
type PipeManager = M.Map SourceID PipeManagerEntry
data PipeOrder = PipeAdd PipeID (Event PipeMessage)
               | PipeDel PipeID

onOrder (PipeAdd pID pE) = M.insert pID pE
onOrder (PipeDel pID) = M.delete pID



{-| Retourne la PipeMap de la source donnée, les events d'order à réactimate et un moyen d'obtenir la commande |-}
newPipeModule :: Handler CryptoOrder
              -> Event (Request, Event PipeMessage)
              -> Reactive PipeManagerEntry --(Behaviour PipeMap, Event (Reactive ()), Handler PipeOrder)
newPipeModule fireCrypto newReqE = do (pipeOrders,fireOrders) <- newEvent
                                      pMapB <- accum M.empty $ onOrder <$> pipeOrders
 
                                      pure $ PipeManagerEntry pMapB (pipeFired fireOrders) fireOrders

 where -- Accepte le pipe s'il n'appartient pas déjà à la pipeMap
       onNewReq :: Handler PipeOrder -> (Request, Event PipeMessage) -> Reactive ()
       onNewReq fire (req,pMsgE) = do fire $ PipeAdd (reqPipeID req) pMsgE
                                      fireCrypto $ CryptoAdd' (reqPipeID req) (reqPipeKey req)
       pipeFired :: Handler PipeOrder -> Event (Reactive ())
       pipeFired fire = onNewReq fire <$> newReqE
       

newPipeManager :: Handler CryptoOrder -> Behaviour RoutingManager -> Reactive (Behaviour PipeManager)
newPipeManager fireCrypto rManaB = swapB $ sequenceA . fmap (newPipeModule fireCrypto ) <$> rManaB

{-| Retourne le flux de data, indépendamment du pipe, transmis par chaque source |-}
pipesStream :: Behaviour PipeManager -> Behaviour (M.Map SourceID (Event PipeMessage))
pipesStream pManaB = fmap (fmap ( srcStream . pmePipeMap) ) pManaB
  where srcStream :: Behaviour PipeMap -> Event PipeMessage
        srcStream pMapB = switchE $ foldr merge never . M.elems <$> pMapB

