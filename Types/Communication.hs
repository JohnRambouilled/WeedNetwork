module Types.Communication where
import Types.Callbacks
import Packets
import qualified Data.Map as M

data ComError = ComError
              | ComErrorExit ComMessage

data ComEntry = ComEntry {comCallback :: Callback ComError ComMessage}
type ComModule = M.Map ComID ComEntry




--public
--
--newComModule :: IO (TVar ComModule, Callback DestinaryError PipeMessage)
