{-# LANGUAGE DeriveGeneric #-}

module Location.Core where

import Data.Aeson (defaultOptions, FromJSON, genericToEncoding, ToJSON, toEncoding)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

data Coordinates = Coordinates {
      latitude :: Double
    , longitude :: Double
    , altitude :: Double
} deriving (Generic, Read, Show)

instance ToJSON Coordinates where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Coordinates

emptyCoordinates :: Coordinates
emptyCoordinates = Coordinates 0.0 0.0 0.0

oaklandCoordinates :: Coordinates
oaklandCoordinates = Coordinates 37.8044 (-122.2711) 13.0

-- TODO: remove this unsafePerformIO. The problem is that this has to be constructed at
-- startup, and there's no way to do 
currentPosition :: TVar Coordinates
currentPosition = unsafePerformIO $ newTVarIO emptyCoordinates

getCurrentPosition :: IO Coordinates
getCurrentPosition = readTVarIO currentPosition

setCurrentPosition :: Double -> Double -> Double -> IO Coordinates
setCurrentPosition lat lon alt = (atomically $ writeTVar currentPosition (Coordinates lat lon alt)) >> readTVarIO currentPosition

setCurrentCoordinates :: Coordinates -> IO Coordinates
setCurrentCoordinates coordinates = (atomically $ writeTVar currentPosition coordinates) >> readTVarIO currentPosition
