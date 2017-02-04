{-# LANGUAGE DeriveGeneric #-}
module Types.Packets where

import Types.Crypto

import Data.Binary
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics

type Time = POSIXTime
type Number = Int
type SourceID = KeyHash
type UserID = KeyHash
type Road = [SourceID]
type TTL = Int
newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord,Generic, Show)

newtype ProtocolID = ProtocolID Int
    deriving (Eq, Ord, Generic, Show)

newtype PipeID = PipeID RawData
    deriving (Eq, Ord, Generic, Show)

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put (toRational t :: Rational)

instance Binary RessourceID
instance Binary ProtocolID
instance Binary PipeID
