module Sources where

import Crypto
import Routing
import Communication
import Pipes


import Data.ByteString.Lazy hiding (split,last)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Control.Event.Handler

type SourceID = Int
data SourceEntry = SourceEntry {seFire :: Handler DataPacket,
                                seNetwork :: EventNetwork}

type SourcesMap = M.Map SourceID SourceEntry

data SourceOrder = SourceAdd SourceID SourceEntry
                 |  SourceDelete SourceID

data NewSourceEntry = NewSourceEntry (Handler ComOrders) (AddHandler ComInit)


buildSourcesManager :: Frameworks t => Event t Request -> (Event t SourceOrder, Handler SourceOrder) -> Handler CryptoOrders -> Handler NewSourceEntry -> Moment t ()
buildSourcesManager reqE (sourceOE,fireSourceOrder) cryptoOrdH newSourceH = reactimate $ (onRequest cryptoOrdH fireSourceOrder newSourceH <$> sourcesMap) <@> reqE
  where sourcesMap = accumB M.empty $ onSourceOrder <$> sourceOE

onRequest :: Handler CryptoOrders -> Handler SourceOrder -> Handler NewSourceEntry -> SourcesMap -> Request -> IO ()
onRequest cryptoOrdH sOrdH newSourceH sMap req = case sID `M.lookup` sMap of
                                                    Just _ -> pure ()
                                                    Nothing -> do (comOrdH,fireOrder) <- newAddHandler
                                                                  (cInitH,fireInit) <- newAddHandler
                                                                  (dataH,fireData) <- newAddHandler
                                                                  net <- compile $ do comOE <- fromAddHandler comOrdH
                                                                                      dataE <- fromAddHandler dataH
                                                                                      buildSource fireInit comOE dataE
                                                                  actuate net

                                                                  newSourceH $ NewSourceEntry fireOrder cInitH
                                                                  cryptoOrdH $ CryptoAdd (reqPipeID req) $ CryptoEntry (reqPipeKey req) fireData
  where sID = last $ reqRoad req


onSourceOrder :: SourceOrder -> SourcesMap -> SourcesMap 
onSourceOrder (SourceAdd sID sEntry) = M.insert sID sEntry
onSourceOrder (SourceDelete sID) = M.delete sID -- TODO faut il pause le network ? Parce que ça c'est de l'IO
