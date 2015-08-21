module Sources where


--import Pipes hiding (onRequest, onOrder)
import Crypto hiding (onOrder)
import Routing hiding (onOrder)

import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Data.List



type SourceMap = M.Map SourceID NewPipeEvent
data SourceOrder = SourceAdd SourceID NewPipeEvent 
                 | SourceDel SourceID


newSourceModule :: Handler CryptoOrder 
                -> Behaviour RoutingManager  -- Events de pipes en fonction des voisins
                -> Reactive (Behaviour SourceMap) -- Events des pipes en fonction des sources
newSourceModule fireCrypto rManaB = sMapB

    where computeSMap :: RoutingManager -> Reactive (Behaviour SourceMap)
          computeSMap = buildSourceModule fireCrypto . foldr merge never . M.elems

          sMapB :: Reactive (Behaviour SourceMap)
          sMapB = join $ switch <$> (hold (pure M.empty) $ execute $ values $ computeSMap <$> rManaB)



onOrder (SourceAdd sID pE) = M.insert sID pE
onOrder (SourceDel sID) = M.delete sID



buildSourceModule :: Handler CryptoOrder -> NewPipeEvent -> Reactive (Behaviour SourceMap)
buildSourceModule fireCrypto newPipeE = do (srcOrder,fire) <- newEvent
                                           sMapB <- accum M.empty $ onOrder <$> srcOrder
                                           -- On accepte tous les pipes qui nous arrivent
                                           listenTrans (filterJust $ snapshot (onRequest fire) newPipeE sMapB) id
                                           pure sMapB
  where -- Ajoute la source si elle n'appartient pas déjà à la sourceMap
        onRequest :: Handler SourceOrder -> NewPipeEventO -> SourceMap -> Maybe (Reactive ())
        onRequest fire (req,pMsgE) sMap = case srcID req `M.lookup` sMap of
                                               Just _ -> Nothing
                                               Nothing -> Just $ do fire $ SourceAdd (srcID req) (extractNewPipes (srcID req) newPipeE)
        srcID = last . reqRoad 


extractNewPipes :: SourceID -> NewPipeEvent -> NewPipeEvent
extractNewPipes sID newPipeE = filterE f newPipeE
        where f (req,_) = head (reqRoad req) == sID

--data SourceEntry = SourceEntry {sePipes :: Behaviour (Event (PipeID,PipeMessage))}
--                                sePipesOrder :: PipeOrder -> Reactive ()}




{-

newSourceModule :: Event Request -- Request qui nous sont destinées
                -> Event PipeMessage -- Flux de pipeMessages
                -> Reactive (Behaviour SourceMap)

newSourceModule reqsE pMsgsE = 

-}


{-
type PipeMessageEvent = Event (PipeID,PipeMessage) -- Censé être le stream actuel des pipeMessages (en fonction de la pipeMap)

type SourceMap = M.Map SourceID PipeMessageEvent

data SourceOrder = SourceAdd SourceID PipeMessageEvent
                 | SourceDel SourceID

dropFirstTuple (_,x,y) = (x,y)

onOrder :: SourceOrder -> SourceMap -> SourceMap
onOrder (SourceAdd sID pMsgE ) = M.insert sID pMsgE
onOrder (SourceDel sID) = M.delete sID





{-| Register une entry à chaque nouvelle source |-}
onRequest :: Event (SourceID,PipeID,PipeMessage) -- PipesMessages provenant de n'importe quelle source
          -> Event Request -- Stream des requêtes provenant de n'importe quelle source
          -> (SourceOrder -> Reactive ())
          -> Request -- Request provenant de n'importe quelle source
          -> SourceMap
          -> Reactive ()
onRequest pMsgsE reqsE fireOrder req sMap = case srcID `M.lookup` sMap of
                                                Just _ -> pure () -- TODO Refresh le pipe
                                                Nothing ->  do 
                                                      msgE <- newPipeModule srcReqsE srcMsgsE
                                                      fireOrder $ SourceAdd srcID msgE
        where srcID = last $ reqRoad req
              srcReqsE = filterE ((srcID==) . last . reqRoad) reqsE -- liste des requêtes provenant de la source
              srcMsgsE = dropFirstTuple <$> filterE (\(src,_,_) -> src == srcID) pMsgsE -- liste des pipeMessages provenant de la source


{-| Retourne un event par source. Chaque occurence correspond à une nouvelle source,
    et contient un stream de pipeMessage (actualisé) provenant d'elle. |-}


newSourceModule :: Event (SourceID,PipeID,PipeMessage) -- Event de tous les pipeMessages de toutes les sources
                -> Event Request -- Event de toutes les requêtes de toutes les sources
                -> Reactive (Behaviour SourceMap) -- Behaviour actualisé

newSourceModule pMsgsE reqsE = do (orderE,fireOrder) <- newEvent
                                  sMapB <- accum M.empty $ onOrder <$> orderE
                                  -- On listen tous les orders provenant des nouvelles requêtes
                                  listenTrans (snapshot (onRequest pMsgsE reqsE fireOrder) reqsE sMapB) id --ATTENTION normalement on ne devrait pas avoir à unregister ça
                                  pure sMapB

-}
