module Types.Communication where
import Packets
import Types.Packets
import qualified Data.Map as M

data ComModule = ComModule {comMap :: M.Map ComID ComEntry,
                            comSource :: SourceID}

data ComEntry = ComEntry {comCallback :: ComMessage -> IO ()}

