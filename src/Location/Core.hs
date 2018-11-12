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

showAccuracy :: Double -> String
showAccuracy acc =
  if acc <= 0 then "(no accuracy data)" else "within " ++ (show acc) ++ "m"

data Coordinates = Coordinates
  { latitude  :: Double
  , longitude :: Double
  , altitude  :: Double

  -- Note: the UTC offset should be stored in the database as well, but for now
  -- I'm going to keep this as is.
  , timestamp :: Int

  -- Finding out that this is useful. Note that -1.0 is used to mean "accuracy not given."
  , accuracy :: Double
  } deriving (Generic, Read)

instance Show Coordinates where
  show (Coordinates lat lon alt timestamp acc) =
    (show $ abs lat) ++
    "°" ++
    (northOrSouth lat) ++
    ", " ++
    (show $ abs lon) ++ "°" ++ (eastOrWest lon) ++ " @ " ++ (show alt) ++ "m " ++ (showAccuracy acc)

instance ToJSON Coordinates where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Coordinates

instance FromRow Coordinates where
  fromRow = Coordinates <$> field <*> field <*> field <*> field <*> field

instance ToRow Coordinates where
  toRow (Coordinates lat lon alt timestamp acc) = toRow (lat, lon, alt, timestamp, acc)

-- getAccuracy

-- The following are some useful test structures.
emptyCoordinates :: Coordinates
emptyCoordinates = Coordinates 0.0 0.0 0.0 0 0

oaklandCoordinates :: Coordinates
oaklandCoordinates = Coordinates 37.8044 (-122.2711) 13.0 0 0

-- This was failing because of a non-exhaustive match in the DB module.
failingJSON :: String
failingJSON
  = "{\"latitude\":37.80608,\"longitude\":-122.2598656,\"altitude\":0,\"timestamp\":1541921713851}"

