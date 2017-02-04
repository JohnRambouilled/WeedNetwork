module Client.Timer where

import Types


createTimer :: Time -> WeedMonad () -> WeedMonad TimerEntry
createTimer t a = createTimerEntry >>= \e -> (startTimer e t a >> pure e)

createTimerEntry :: WeedMonad TimerEntry
createTimerEntry = undefined

startTimer :: TimerEntry -> Time -> WeedMonad () -> WeedMonad ()
startTimer = undefined

startTimerIO :: TimerEntry -> Time -> IO () -> WeedMonad ()
startTimerIO = undefined

refreshTimer :: TimerEntry -> WeedMonad ()
refreshTimer = undefined

killTimer :: TimerEntry -> WeedMonad ()
killTimer = undefined



