module Proxy.RoadChoice where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Data.Binary

import Client.Packet
import Client
import Client.Sources
import Client.Ressource
import Client.Pipes
import Log

nbRoads = 1
nbSourcesMax = 1
inetRessourceID :: RessourceID
inetRessourceID = RessourceID $ encode "www"



{-| Always keeps k roads to the internet |-}
proxyRoadChoice :: MVar Sources -> MVar [SourceID] -> RoadChoice
proxyRoadChoice sourcesV sIDsV  = RoadChoice roadC
  where roadC road sID cert raw = do 
                                     sIDs <- liftIO $ readMVar sIDsV
                                     keepLog ProxyLog Normal $ "[Roads] : RoadChoice on road : " ++ show (roadToRoadID road)
                                     keepLog ProxyLog Normal $ "[Roads] : Registered roads are : " ++ show sIDs
                                     if length sIDs < nbSourcesMax 
                                         then case sID `elem` sIDs of
                                                False -> pure True
                                                True -> maybe False (roadToRoadID road `notElem`) <$> extractRoads sourcesV sID
                                         else return False

                                                                                
choseDest :: MVar Sources -> MVar [SourceID] -> IOLog (Maybe SourceEntry)
choseDest sourcesV sIDs = (liftIO $ safeHead <$> readMVar sIDs) >>= maybe (pure Nothing) (getSourceEntry sourcesV)  
  where safeHead [] = Nothing
        safeHead (x:_) = Just x


