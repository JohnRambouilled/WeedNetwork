

listenRessource :: RessourceID -> IO (TChan SourceID)

getRessourceSources :: RessourceID -> IO [SourceID]

sourceGetRoads :: CleverParameter -> SourceID -> IO [Road]

openRoad :: SourceID -> Road -> IO PipeID

getSourcePipes :: SourceID -> IO [PipesID]

openCommunication :: SourceID -> ProtocolID -> IO (Socket RawData)   --Maybe? if no road open? if source disapeared?


class Closable a where close :: a -> IO ()

instance Closable SourceID
instance Closable PipeID

