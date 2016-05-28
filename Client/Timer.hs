module Client.Timer where

import Types


newTimerEntry :: STMIO () -> STMIO TimerEntry
newTimerEntry kill = pure $ TimerEntry (pure ()) (pure ()) 



