module Sources where

import Pipes hiding (onRequest, onOrder)

import FRP.Sodium
import FRP.Sodium.Internal hiding (Event)
import Data.ByteString.Lazy (ByteString)
import GHC.Generics
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Data.List


data SourceEntry = SourceEntry {sePipes :: Behaviour (Event (PipeID,PipeMessage))}
--                                sePipesOrder :: PipeOrder -> Reactive ()}



{-

newSourceModule :: Event Request -- Request qui nous sont destinées
                -> Event PipeMessage -- Flux de pipeMessages
                -> Reactive (Behaviour SourceMap)

newSourceModule reqsE pMsgsE = 

-}


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
