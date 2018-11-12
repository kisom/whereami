{-# LANGUAGE OverloadedStrings #-}

module Location.DB where

import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Database.SQLite.Simple        as SQLite
import qualified Location.Core                 as Core
import qualified System.Environment            as Env


getTimestamp :: IO Int
getTimestamp = (round . (* 1000)) <$> getPOSIXTime

database :: IO String
database = do
  database_ <- Env.lookupEnv "WHEREAMI_DB"
  return $ case database_ of
    Just p  -> p
    Nothing -> "whereami.db"

setupDatabase :: IO SQLite.Connection
setupDatabase = do
  database <- database
  conn     <- SQLite.open database
  SQLite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS whereami (id INTEGER PRIMARY KEY AUTOINCREMENT, lat REAL, lon REAL, alt REAL, timestamp INTEGER NOT NULL, accuracy REAL DEFAULT 0.0, altitude_accuracy REAL DEFAULT 0.0);"
  return conn

coordinatesWithTimestamp :: Core.Coordinates -> IO Core.Coordinates
coordinatesWithTimestamp (Core.Coordinates lat lon alt 0 gacc aacc) = do
  timestamp <- getTimestamp
  return $ Core.Coordinates lat lon alt timestamp gacc aacc
coordinatesWithTimestamp c = return c

-- followup: https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html
storeCoordinates :: SQLite.Connection -> Core.Coordinates -> IO ()
storeCoordinates conn c = do
  putStrLn "some shit is about to go down"
  coords <- coordinatesWithTimestamp c
  putStrLn "some shit went down"
  SQLite.execute
    conn
    "INSERT INTO whereami (lat, lon, alt, timestamp, accuracy, altitude_accuracy) VALUES (?, ?, ?, ?, ?, ?)"
    coords

getCoordinates :: SQLite.Connection -> IO Core.Coordinates
getCoordinates conn = do
  r <-
    SQLite.query_
      conn
      "SELECT lat, lon, alt, timestamp, accuracy, altitude_accuracy FROM whereami ORDER BY timestamp DESC LIMIT 1;" :: IO
      [Core.Coordinates]
  return $ coordinateResult r
 where
  coordinateResult (h : _) = h
  coordinateResult []      = Core.emptyCoordinates

shutdown :: SQLite.Connection -> IO ()
shutdown conn = SQLite.close conn
