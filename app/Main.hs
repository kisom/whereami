{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.SecureMem
import qualified Location.API as API
import qualified Location.DB as DB
import Network.Wai.Middleware.HttpAuth
import Web.Scotty

main :: IO ()
main = do
  conn <- DB.setupDatabase
  scotty 4000 $ do
    middleware $ basicAuth (\u p -> return $ u == "kyle" && secureMemFromByteString p == API.authPassword) "Where am I?"
    get "/" $ file "page.html"
    get "/coordinates" $ API.getCoordinates conn
    post "/coordinates" $ API.postCoordinates conn
    notFound $ API.notFound
  DB.shutdown conn