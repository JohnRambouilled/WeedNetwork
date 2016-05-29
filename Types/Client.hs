module Types.Client where

import Types.Callbacks
import Types.Neighbours
import Types.Pipes
import Types.Destinary
import Control.Concurrent.STM (TVar)

data Client = Client { clNeighbour :: TVar NeighbourModule,
                       clPipes     :: TVar PipesModule,
                       clDestinary :: TVar DestinaryModule,
                       clReader    :: STMReader}
