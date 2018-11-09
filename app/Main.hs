{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Location.API as API
import Web.Scotty

main :: IO ()
main = do
  scotty 4000 $ do
    get "/coordinates" $ API.getCoordinates
