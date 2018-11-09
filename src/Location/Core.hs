{-# LANGUAGE DeriveGeneric #-}

module Location.Core where

import Data.Aeson (defaultOptions, FromJSON, genericToEncoding, ToJSON, toEncoding)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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

-- TODO: remove this unsafePerformIO. The problem is that this has to be constructed at
-- startup, and there's no way to do 
currentPosition :: IORef Coordinates
currentPosition = unsafePerformIO . newIORef $ emptyCoordinates

getCurrentPosition :: IO Coordinates
getCurrentPosition = readIORef currentPosition

setCurrentPosition :: Double -> Double -> Double -> IO Coordinates
setCurrentPosition lat lon alt = writeIORef currentPosition (Coordinates lat lon alt) >> readIORef currentPosition

-- toCoordinates :: Geo.Geodetic Geo.WGS84 -> IO Coordinates
-- toCoordinates (Geo.Geodetic lat lon alt Geo.WGS84) = do
--     lat' <- toExactRational $ exactValue lat
--     return $ Coordinates lat' 0.0 0.0
-- 
-- return $ Coordinates lat' lon' alt'
--   where lat' = fromRational $ exactValue lat
--         lon' = fromIntegral lon
-- 	alt' = fromIntegral alt
