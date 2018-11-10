{-# LANGUAGE OverloadedStrings #-}

module Location.DB where

import Database.SQLite.Simple as SQLite
import qualified Location.Core as Core

-- General database design notes:
--
-- 1. There's really only one user - me. So, it makes sense to
--    open the database each time, read/write, and close it.

database :: String
database = "whereami.db"

setupDatabase :: IO SQLite.Connection
setupDatabase = do 
    conn <-  SQLite.open database
    SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS whereami (id INTEGER PRIMARY KEY AUTOINCREMENT, lat REAL, lon REAL, alt REAL, timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP);"
    return conn

-- followup: https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html
storeCoordinates :: SQLite.Connection -> Core.Coordinates -> IO ()
storeCoordinates conn c = do
    SQLite.execute conn "INSERT INTO whereami (lat, lon, alt) VALUES (?, ?, ?)" c

getCoordinates :: SQLite.Connection -> IO Core.Coordinates
getCoordinates conn = do
    r <- SQLite.query_ conn "SELECT lat, lon, alt FROM whereami ORDER BY timestamp DESC LIMIT 1;" :: IO [Core.Coordinates]
    return $ coordinateResult r
  where coordinateResult (h:_) = h
	coordinateResult [] = Core.emptyCoordinates

shutdown :: SQLite.Connection -> IO ()
shutdown conn = SQLite.close conn