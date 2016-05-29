module Types.Sendable where

import Packets
import Types.Callbacks
import Types.Crypto
import Client.Crypto

import Control.Monad
import Control.Monad.Reader

class Sendable a where send_ :: a -> STMIO RawPacket

instance Sendable PipePacket where send_ = pure . RawPipePacket
instance Sendable NeighData where send_ = pure . RawNeighData
instance Sendable NeighDataContent where send_ ndc = do me <- whoAmI 
                                                        keyPair <- lift $ asks stmKeys 
                                                        send_ $ sign keyPair $ NeighData me emptySignature ndc
instance Sendable Request where send_ = send_ . NeighReq



send :: Sendable a => a -> STMIO ()
send p = join . lift $ asks f
    where f :: STMReader -> STMIO ()
          f reader = stmSender reader =<< send_ p

