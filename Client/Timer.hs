module Client.Timer where

import Types
import Client.WeedMonad

import Control.Lens
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM


timeToMicroseconds :: Time -> Int
timeToMicroseconds = round . (*10^6) 

refreshTimer :: TimerEntry -> WeedMonad ()
refreshTimer = startTimer

newTimer :: Time -> WeedMonad () -> WeedMonad TimerEntry
newTimer t w = newTimerIO t =<< runWM' w

newTimerIO :: Time -> IOAction -> WeedMonad TimerEntry
newTimerIO t a = do e <- newTimerEntryIO t a
                    startTimer e
                    pure e

-- | Might not be great... 
-- | The timer is first killed if it was running. We then push an IOAction which start the timer thread if the timer isn't already running, and write the threadID in the TVar.
-- | The timer thread wait the specified time, and then execute the INITIAL action if a threadID is still written in the TVar.
-- | 
-- |    This is done to avoid issue if a timer tick between a call to killTimer and before the actual killThread call.
-- |    The check in ioA is done to prevent multiple timer from being launch, as the threadID is written only after the transaction.
startTimer :: TimerEntry -> WeedMonad ()
startTimer eV = killTimer' eV >>= weedIO . ioA
    where ioA e = do e' <- readTVarIO eV
                     case _timerThreadID e' of Just _ -> pure ()
                                               Nothing ->  atomically . modifyTVar eV . set timerThreadID . Just =<< forkIO (timerA e)
          timerA e = do threadDelay . timeToMicroseconds $ _timerDelay e
                        e' <- readTVarIO eV
                        case _timerThreadID e' of Nothing -> pure ()
                                                  Just _ -> _timerAction e


newTimerEntry_ :: WeedMonad TimerEntry
newTimerEntry_ = newTimerEntry 1 $ pure ()


newTimerEntry :: Time -> WeedMonad () -> WeedMonad TimerEntry
newTimerEntry t a = newTimerEntryIO t =<< runWM' a

-- | Create a new TimerEntry with initial value for a delay and a IO action. Does NOT start the timer.
newTimerEntryIO :: Time -> IOAction -> WeedMonad TimerEntry
newTimerEntryIO t a = liftSTM . newTVar $ TimerEntry' Nothing t a

setTimerEntry :: TimerEntry -> Time -> WeedMonad () -> WeedMonad ()
setTimerEntry e t a = setTimerEntryIO e t =<< runWM' a

-- | set the delay and action of the TimerEntry. Does not modify the current instances if the timer is already running.
setTimerEntryIO :: TimerEntry -> Time -> IOAction -> WeedMonad ()
setTimerEntryIO eV t a = modifySTM eV $ set timerDelay t . set timerAction a

-- | set the delay of the TimerEntry. Does not modify the current instances if the timer is already running.
setTimerDelay :: TimerEntry -> Time -> WeedMonad ()
setTimerDelay eV = modifySTM eV . set timerDelay

setTimerAction :: TimerEntry -> WeedMonad () -> WeedMonad ()
setTimerAction t a = setTimerActionIO t =<< runWM' a

-- | set the action of the TimerEntry. Does not modify the current instances if the timer is already running.
setTimerActionIO :: TimerEntry -> IOAction -> WeedMonad ()
setTimerActionIO eV = modifySTM eV . set timerAction

killTimer :: TimerEntry -> WeedMonad ()
killTimer = void . killTimer'

-- | Stop a timer. The TimerEntry is modified immediately, but the actual timer will only be stopped after the transaction.
-- | If the timer isn't running, it, surprisingly enough, does do anything.
killTimer' :: TimerEntry -> WeedMonad TimerEntry'
killTimer' eV = do e <- readSTM eV
                   case _timerThreadID e of 
                        Nothing -> pure e
                        Just id -> do writeSTM eV . set timerThreadID Nothing $ e
                                      weedIO $ killThread id
                                      pure e


