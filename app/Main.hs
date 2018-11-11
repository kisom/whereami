{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8          ( pack )
import           Data.SecureMem
import qualified Location.API                  as API
import qualified Location.DB                   as DB
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Middleware.HttpAuth
import           Network.Wai.Middleware.RequestLogger
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
  conn <- DB.setupDatabase
  user <- getUser
  putStrLn $ "User: " ++ user
  port <- getPort
  scotty port $ do
    middleware logStdoutDev
    middleware $ basicAuth
      (\u p ->
        return $ u == pack user && secureMemFromByteString p == API.authPassword
      )
      "Where am I?"
    get "/" $ API.staticPage "static/index.html"
    get "/coordinates" $ API.getCoordinates conn
    post "/coordinates" $ API.postCoordinates conn
    notFound $ API.notFound
  DB.shutdown conn
