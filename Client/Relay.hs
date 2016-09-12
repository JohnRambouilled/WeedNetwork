module Client.Relay where

import Types
import Packets
import Client.Pipes
import Client.Crypto

import Control.Concurrent.STM

data ReqError = ReqError



onRequest
  :: TVar PipesModule
     -> Callback ReqError Request
     -> Callback NeighError (NeighID, Request)
onRequest pipesMod = splitRequest $ Callback $ relayRequest pipesMod

splitRequest :: Callback ReqError Request -> Callback ReqError Request -> Callback NeighError (NeighID,Request)
splitRequest relayReq localReq = Callback $ callback
    where callback :: Either NeighError (NeighID,Request) -> STMIO ()
          callback (Left err) = error "Relay : NeighError reçu"
          callback (Right (neighID , req)) = do (me,time) <- (,) <$> whoAmI <*> getTime
                                                case checkRequest  me time (neighID,req) of
                                                   Left str -> logM $ RawLog str
                                                   Right _  -> let pos = reqPosition req
                                                                   len = reqLength req
                                                               in if pos == 1 || pos == len - 1 then runCallback localReq (Right req)
                                                                  else runCallback relayReq (Right req)


relayRequest :: TVar PipesModule -> Either ReqError Request -> STMIO ()
relayRequest _ (Left err) = error "Relay : ReqError reçu"
relayRequest pipesMod (Right req) = do registerPipe pipesMod req $ Callback relayCallback
                                       send req{reqPosition = 1 + reqPosition req}
    where relayCallback :: Either PipeError PipeData -> STMIO ()
          relayCallback (Right pkt) = send pkt{pipeDPosition = newDPos pkt}
          relayCallback (Left (PipeClosedError pkt)) = send pkt{pipeCPosition = newCPos pkt}
          newDPos pkt = pipeDPosition pkt + (if pipeDDirection pkt then 1 else -1)
          newCPos pkt = pipeCPosition pkt + (if pipeCDirection pkt then 1 else -1)

checkRequest :: UserID -> Time -> (NeighID,Request) -> (Either String Request)
checkRequest = checkReq
    where checkReq me t (neighID,req@(Request n l r epk t' pK pH s c))
            | l > roadLengthMax                 = Left "Rejected road : too long"
            | n > l-1                           = Left "Incorrect RequestPosition"
            | l /= length r                      = Left "Incorrect RoadLength"
            | t - t' > maxDelay                 = Left "Obsolete Request" 
            | r !! n /= me                       = Left "Not adressed to me"
            | r !! (n-1) /= neighID             =  Left "The previous neighbour does not match the road"
            | computeHashFromKey pK /= pH        = Left "Invalid tuple KeyHash/PubKey"
            | checkSig pK req                   = Right req
            | otherwise                         = Left "Invalid signature"



