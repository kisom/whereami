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
  } deriving (Generic, Read)

instance Show Coordinates where
  show (Coordinates lat lon alt) =
    (show $ abs lat) ++
    "°" ++
    (northOrSouth lat) ++
    ", " ++
    (show $ abs lon) ++ "°" ++ (eastOrWest lon) ++ " @ " ++ (show alt) ++ "m"

instance ToJSON Coordinates where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Coordinates

instance FromRow Coordinates where
  fromRow = Coordinates <$> field <*> field <*> field

instance ToRow Coordinates where
  toRow (Coordinates lat lon alt) = toRow (lat, lon, alt)

emptyCoordinates :: Coordinates
emptyCoordinates = Coordinates 0.0 0.0 0.0

oaklandCoordinates :: Coordinates
oaklandCoordinates = Coordinates 37.8044 (-122.2711) 13.0
