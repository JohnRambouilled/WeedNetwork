{-# LANGUAGE DeriveGeneric #-}
module Pipes where

import Crypto
import Routing 
import Class


import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M

type UserID = KeyHash

type PipesMap = EventMap PipeID PipeMessage
type PipesManager = EventManager UserID PipeID PipeMessage
type PipesManagerBhv = Behavior PipesManager

type PipesSender = Handler PipeMessage
type PipesSenderMap = M.Map PipeID PipesSender
type PipesSenderManager = M.Map UserID (Behavior PipesSenderMap)
type PipesSenderManagerBhv = Behavior PipesSenderManager

data Pipes = Pipes {pipesManager :: PipesManager,
                    pipesSenders :: PipesSenderManager,
                    pipesCloser :: Handler PipeID}

buildPipeManager :: Event NewPipe -> Reactive (PipesManagerBhv, Handler PipeID)
buildPipeManager newPipeE = do (closeE, closeH) <- newEvent
                               pMan <- accum M.empty $ merge (openPipe <$> newPipeE) (closePipe <$> closeE)
                               pure (pMan, closeH)
    where openPipe _ = id
          closePipe _ = id

newSourceEvent :: Event NewPipe -> PipesManagerBhv -> Event PipesMap
newSourceEvent npE pMbhv = filterJust $ snapshot makePipesMapMaybe npE pMbhv
    where makePipesMapMaybe :: NewPipe -> PipesManager -> Maybe PipesMap
          makePipesMapMaybe (r, e) pMan = case sID `M.lookup` pMan of
                                            Just _ -> Nothing
                                            Nothing -> Just (M.singleton sID e)
                                        where sID = head $ reqRoad r

{-| Converts the map of physical neighbors into a map of recipients |-}
type RecipientMap = EventEntryMap SourceID NewPipe
buildRecipientMap :: Event NewPipe -> Reactive (Behaviour RecipientMap)
buildRecipientMap npE = do rMapB <- accum (EventEntryMap M.empty) (execute $ f <$> npE)
                           listenTrans (filterJust $ snapshot (\np m -> fireKey' m (last $ reqRoad $ fst np) np) npE rMapB) id
                           pure rMapB
  where f :: (Request, Event PipeMessage) ->  Reactive (RecipientMap -> RecipientMap)
        f (req, stream) = (insertEntry (last $ reqRoad req)  <$> newEventEntry (pure True) )

{-| Manages the pipes for a given recipient |-}
type PipeMap = EventEntryMap PipeID PipeMessage
buildPipeMap :: Event NewPipe -> Reactive (Behaviour PipeMap)
buildPipeMap npE = 


{-
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
-}
