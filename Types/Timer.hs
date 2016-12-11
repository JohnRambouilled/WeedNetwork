module Types.Timer where
import Types.Packets

import Control.Concurrent.STM.TMVar
import Control.Concurrent

type TimerRefresh = IO ()
type TimerKill = IO ()
data TimerEntry = TimerEntry {timerEntryID :: TMVar ThreadId }


