module Types.Communication where
import Packets
import qualified Data.Map as M

type ComMap = M.Map ComID ComEntry

data ComEntry = ComEntry {comCallback :: ComMessage -> IO ()}

