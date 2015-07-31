module Pipes where
import Crypto
import Routing
import Communication

import Data.ByteString.Lazy hiding (split,last)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Control.Event.Handler

type PipeID = KeyHash
type PipeMap = M.Map PipeID PipeEntry
type PipeEntry = Handler RawData

data PipeOrders = PipeAdd PipeID PipeEntry
                | PipeDelete PipeID



buildSource :: Frameworks t => Handler ComInit -> Event t ComOrders -> Event t DataPacket ->  Moment t ()
buildSource comIH ordE dataE = do (pipeOE, pipeOH) <- newEvent
                                  comE <- buildPipes pipeOE pipeOH dataE
                                  buildCommunication comIH --todo




buildPipes :: Frameworks t => Event t PipeOrders -> Handler PipeOrders -> Event t DataPacket -> Moment t (Event t ComPacket)
buildPipes orderE fireOrder dataE = 
                                      do let mapBhv = accumB M.empty (onPipeOrder <$> orderE)
                                             (aE, cpE) = split (onPipePacket fireOrder <$> dataE)
                                         reactimate aE
                                         pure cpE


onPipePacket :: Handler PipeOrders -> DataPacket -> Either (IO ()) ComPacket
onPipePacket ordH dP = case decodeOrFail (signedPayload $ dataContent dP) of
                        Left _  -> Left (pure ())
                        Right (_,_,pM) -> onPipeMessage pM
     where onPipeMessage (PipeData _ d) = case decodeOrFail d of
                                                Left _ -> Left (pure ())
                                                Right (_,_,cM) -> Right cM
           onPipeMessage (PipeExit _ _) = Left . ordH $ PipeDelete (datakeyID dP)


onPipeOrder :: PipeOrders -> PipeMap -> PipeMap
onPipeOrder (PipeAdd pID pE) = M.insert pID pE
onPipeOrder (PipeDelete pID) = M.delete pID

