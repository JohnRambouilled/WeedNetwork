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
type PipeID = KeyHash
type TTL = Int
newtype RessourceID = RessourceID RawData
    deriving (Eq,Ord,Generic, Show)


-- TODO
ttlMax = 10
ansMaxDelay = 10

relayTimeOut = 10 :: Time


roadLengthMax = 10 :: Number
maxDelay = 20 :: Time

instance Binary NominalDiffTime where
        get = fromRational <$> get
        put t = put (toRational t :: Rational)

instance Binary RessourceID
