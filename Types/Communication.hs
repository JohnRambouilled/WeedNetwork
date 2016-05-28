module Types.Communication where

import Types.Callbacks
import qualified Data.Map as M

data ComError = ComError
data ComMessage = ComMessage

data ComID = ComID Int
data ComEntry = ComEntry {
                          comCallback :: Callback ComError ComMessage}
data ComModule = ComModule {runControlComModule :: M.Map ComID ComEntry}

--public
--
--newComModule :: IO (TVar ComModule, Callback DestinaryError PipeMessage)
