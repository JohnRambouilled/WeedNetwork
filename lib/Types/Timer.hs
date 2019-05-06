{-# LANGUAGE TemplateHaskell   #-}
module Types.Timer where
import Types.Packets

import Control.Concurrent.STM
import Control.Concurrent

import Control.Lens

type TimerEntry = TVar TimerEntry'

data TimerEntry' = TimerEntry' {_timerThreadID :: Maybe ThreadId,
                                _timerDelay :: Time,
                                _timerAction :: IO ()}


makeLenses ''TimerEntry'


