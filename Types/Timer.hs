module Types.Timer where
import Types.Packets

import Control.Concurrent.STM.TMVar
import Control.Concurrent

newtype TimerRefresh = TimerRefresh (IO ())
newtype TimerKill = TimerKill (IO ())
data TimerEntry = TimerEntry {timerEntryID :: TMVar ThreadId }


