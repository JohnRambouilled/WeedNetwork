module Sources where

import Crypto
import Routing

import Data.ByteString.Lazy hiding (split,last)
import Data.Binary
import Control.Monad
import qualified Data.Map as M
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch
import Control.Event.Handler

type PipeID = KeyHash
type PipePubKey = PubKey
type PipePrivKey = PrivKey
type SourceID = KeyHash
type ComID = Int

data PipeEntry = PipeEntry {pePubKey :: PipePubKey,
                            pePrivKey :: PipePrivKey}

data ComEntry = ComEntry {ceFire :: ComCalback}
data Communication = Communication { comMap :: M.Map ComID ComEntry,
                                     currentComID :: ComID }

data SourceEntry = SourceEntry { sePipes :: M.Map PipeID PipeEntry, -- Pipe keys
                                 seCommunications :: Communication}

type SourceMap = M.Map SourceID SourceEntry

data SourceOrder = SourceAdd SourceID SourceEntry
                 | SourceDelete SourceID PipeID
                 | SourceAddComID SourceID ComID ComEntry
                 | SourceDelComID SourceID ComID


data ComMessage = ComInit {cmID :: ComID, cmPayload :: RawData}
                | ComData {cmID :: ComID, cmPayload :: RawData}
                | ComExit {cmID :: ComID}

isComExit (ComExit _) = True
isComExit _ = False
isComData (ComData _ _) = True
isComData _  = False
isComInit = not <$> ((||) <$> isComExit <*> isComData)

data ComError = ComError RawData

type ComCalback = Handler (Either ComError RawData)

newCommunication :: Communication
newCommunication = Communication M.empty 0


{-| Sends every comInit to the default handler and route every messages to the entry, if it exists. |-}
manageComMessages :: SourceMap -> SourceID 
                  -> Handler SourceOrder --Channel for source orders
                  -> Handler ComMessage -- Channel for higher layers (called on new comIDs)
                  -> Handler ComMessage
manageComMessages sMap sID sOrdH comH msg = case sID `M.lookup` sMap of
                                                Nothing -> pure ()
                                                Just sEntry -> case cmID msg `M.lookup` comMap (seCommunications sEntry) of
                                                                   Just (ComEntry fire) -> if isComExit msg then fire $ Left (ComError $ cmPayload msg)
                                                                                            else if isComData msg then fire $ Right $ cmPayload msg
                                                                                                                  else pure ()
                                                                   Nothing -> if isComInit msg then do comH $ msg
                                                                                               else pure ()


{-
onRequest :: PrivKey -> Request -> SourceMap -> Handler SourceOrder -> Handler CryptoOrders -> Handler ComMessage -> IO ()
onRequest mePrv (Request _ _ road epk _ pKey pH _ d) srcM sOrdH cOrdH comMessageH = case decodeOrFail $ decrypt mePrv epk of
                                                            Left (_,_,err) -> print err
                                                            Right (_,_,(privPipeH,privPipeK)) -> when (computeHashFromKey privPipeK == (privPipeH :: KeyHash)) $
                                                                                                    let pEntry = PipeEntry pKey privPipeK
                                                                                                    in case sID `M.lookup` srcM of 
                                                                    Just sEntry -> case pH `M.lookup` sePipes sEntry of
                                                                                     Just _ -> pure ()
                                                                                     Nothing -> do sOrdH $ SourceAdd sID sEntry{sePipes = M.insert pH pEntry $ sePipes sEntry}
                                                                                                   cOrdH $ CryptoAdd pH $ CryptoEntry pKey managePipeData

                                                                    Nothing -> sOrdH $ SourceAdd sID $ SourceEntry (M.insert pH pEntry M.empty) newCommunication
                                                                              cOrdH $ CryptoAdd pH $ CryptoEntry pKey managePipeData 
        where  sID = last road
               managePipeData (DataPacket kH _ raw) = case decodeOrFail $ signedPayload raw of
                                                        Left (_,_,err) -> print err
                                                        Right (_,_,(PipeData _ d)) -> manageComMessage d
                                                        Right (_,_,(PipeExit _ _)) -> do sOrdH $ SourceDelete sID kH
                                                                                         cOrdH $ CryptoDelete kH

               manageComMessage raw = case decodeOrFail raw of
                                        Left (_,_,err) -> print err
                                        Right (_,_,(ComInit cID rData))


-}
{-
onRequest :: PrivKey -> Request -> SourceMap -> Handler SourceOrder -> Handler CryptoOrders -> IO ()
onRequest mePrv (Request _ _ road epk _ pKey pH _ d) srcM sOrdH cOrdH = case decodeOrFail $ decrypt mePrv epk of
                                                            Left (_,_,err) -> print err
                                                            Right (_,_,(privPipeH,privPipeK)) -> when (computeHashFromKey privPipeK == (privPipeH :: KeyHash)) $
                                                                                                    let pEntry = PipeEntry pKey privPipeK
                                                                                                    in case sID `M.lookup` srcM of 
                                                                    Nothing -> do (addH,fire) <- newAddHandler
                                                                                  sOrdH $ SourceAddNew sID $ SourceEntry (M.insert pH pEntry M.empty) addH fire
                                                                                  cOrdH $ CryptoAdd pH $ CryptoEntry pKey (managePipeData fire)
                                                                    Just sEntry -> case pH `M.lookup` sePipes sEntry of
                                                                                     Just _ -> pure ()
                                                                                     Nothing -> do sOrdH $ SourceAddNew sID sEntry{sePipes = M.insert pH pEntry $ sePipes sEntry}
                                                                                                   cOrdH $ CryptoAdd pH $ CryptoEntry pKey (managePipeData $ seFire sEntry)

                                                                                                          

    where sID = last road
          managePipeData :: Handler RawData -> Handler DataPacket
          managePipeData fire (DataPacket kH _ raw) = case decodeOrFail $ signedPayload raw of
                                                       Left (_,_,err) -> print err
                                                       Right (_,_,(PipeData _ d)) -> fire d
                                                       Right (_,_,(PipeExit _ _)) -> do sOrdH $ SourceDeletePipe sID kH
                                                                                        cOrdH $ CryptoDelete kH


                                                                     

onSourceOrder :: SourceOrder -> SourceMap -> SourceMap
onSourceOrder (SourceAddNew sID sEntry) srcM = M.insert sID sEntry srcM
onSourceOrder (SourceDeletePipe sID pID) srcM = M.adjust (\sEntry -> sEntry{} ) sID srcM
-}
