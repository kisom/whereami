{-# LANGUAGE DeriveGeneric #-}

module Location.Core where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , defaultOptions
                                                , genericToEncoding
                                                , toEncoding
                                                )
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           GHC.Generics

northOrSouth :: Double -> String
northOrSouth d = if d >= 0 then "N" else "S"

eastOrWest :: Double -> String
eastOrWest d = if d >= 0 then "E" else "W"

data Coordinates = Coordinates
  { latitude  :: Double
  , longitude :: Double
  , altitude  :: Double

  -- Note: the UTC offset should be stored in the database as well, but for now
  -- I'm going to keep this as is.
  , timestamp :: Int
  } deriving (Generic, Read)

instance Show Coordinates where
  show (Coordinates lat lon alt timestamp) =
    (show $ abs lat) ++
    "°" ++
    (northOrSouth lat) ++
    ", " ++
    (show $ abs lon) ++ "°" ++ (eastOrWest lon) ++ " @ " ++ (show alt) ++ "m " ++ (show timestamp)

instance ToJSON Coordinates where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Coordinates

instance FromRow Coordinates where
  fromRow = Coordinates <$> field <*> field <*> field <*> field

instance ToRow Coordinates where
  toRow (Coordinates lat lon alt timestamp) = toRow (lat, lon, alt, timestamp)

-- The following are some useful test structures.
emptyCoordinates :: Coordinates
emptyCoordinates = Coordinates 0.0 0.0 0.0 0

oaklandCoordinates :: Coordinates
oaklandCoordinates = Coordinates 37.8044 (-122.2711) 13.0 0

-- This was failing because of a non-exhaustive match in the DB module.
failingJSON :: String
failingJSON
  = "{\"latitude\":37.80608,\"longitude\":-122.2598656,\"altitude\":0,\"timestamp\":1541921713851}"
