module Client.Timer where

import Types


createTimer :: Time -> WeedMonad () -> WeedMonad TimerEntry
createTimer = undefined

createTimerIO :: Time -> IO () -> WeedMonad TimerEntry
createTimerIO = undefined

refreshTimer :: TimerEntry -> WeedMonad ()
refreshTimer = undefined

killTimer :: TimerEntry -> WeedMonad ()
killTimer = undefined



