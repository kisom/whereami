{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8          ( pack )
import           Data.SecureMem
import qualified Location.API                  as API
import qualified Location.DB                   as DB
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.HttpAuth
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import qualified System.Environment            as Env
import           Web.Scotty

getPort :: IO Warp.Port
getPort = do
  port_ <- Env.lookupEnv "WHEREAMI_PORT"
  return $ case port_ of
    Just p  -> read p
    Nothing -> 4000

getUser :: IO String
getUser = do
  user_ <- Env.lookupEnv "WHEREAMI_USER"
  return $ case user_ of
    Just u  -> u
    Nothing -> "kyle"

main :: IO ()
main = do
  -- Get all the configuration data needed.
  conn <- DB.setupDatabase
  user <- getUser
  putStrLn $ "User: " ++ user
  password <- API.getAuthPassword
  port     <- getPort

  scotty port $ do
    -- Set up all the middleware.
    middleware logStdoutDev
    middleware simpleCors
    middleware $ staticPolicy (addBase "assets")
    middleware $ basicAuth
      (\u p -> return $ u == pack user && secureMemFromByteString p == password)
      "Where am I?"

    -- Define the routes.
    get "/coordinates" $ API.getCoordinates conn
    post "/coordinates" $ API.postCoordinates conn
    get "/" $ API.staticPage "static/index.html"
    notFound $ API.notFound

  -- Cleanup.
  DB.shutdown conn
