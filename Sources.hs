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
