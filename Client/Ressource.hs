module Client.Ressource where

import Packets.Ressource
import Client.Crypto








checkAnswer :: UserID -> Time -> Answer -> Either String Answer
checkAnswer me = checkAns me
    where checkAns me t ans@(Answer (RessourceCert _ pK ts val _ _) ttl r sID cnt)
            | ttl <= 1 || ttl > ttlMax      = Left "Incorrect TTL"
            | me `Prelude.elem` r          = Left "Already in the road" 
            | t - ts > val                 = Left "Answer obsolete"
            | computeHashFromKey pK /= sID  = Left "SourceID does not match PublicKey"
            | checkSig pK ans              = Right (relayAnswer ans)
            | otherwise                    = Left "Incorrect signature"
          relayAnswer :: Answer -> Answer
          relayAnswer a = a{ansTTL = ansTTL a - 1, ansRoad = me : ansRoad a}


