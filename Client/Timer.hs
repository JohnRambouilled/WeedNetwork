module Client.Timer where

import Types

createTimer :: Time -> IO () -> WeedMonad TimerEntry
createTimer = undefined

refreshTimer :: TimerEntry -> WeedMonad ()
refreshTimer = undefined

killTimer :: TimerEntry -> WeedMonad ()
killTimer = undefined



