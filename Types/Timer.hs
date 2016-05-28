module Types.Timer where

import Types.Callbacks

type TimerRefresh = STMIO ()
type TimerKill = STMIO ()
data TimerEntry = TimerEntry {timerRefresh :: TimerRefresh,
                              timerKill :: TimerKill}




