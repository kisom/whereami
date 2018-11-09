{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Location.API where

import Data.Aeson (defaultOptions, FromJSON, genericToEncoding, ToJSON, toEncoding)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Generics
import qualified Location.Core as Core
import System.IO.Unsafe (unsafePerformIO)
import Web.Scotty

data Response = Response {
      success :: Bool
    , coordinates :: Core.Coordinates
    , errorMessage :: String
} deriving (Generic, Read, Show)

instance ToJSON Response where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

getCoordinates :: ActionM ()
getCoordinates = json $ Response True coordinates ""
  where coordinates = unsafePerformIO $ Core.getCurrentPosition 

postCoordinates :: ActionM ()
postCoordinates = do 
    coordinates <- jsonData :: ActionM Core.Coordinates
    json . unsafePerformIO $ Core.setCurrentCoordinates coordinates
    json $ Response True coordinates ""

notFound :: ActionM ()
notFound = json $ Response False Core.emptyCoordinates "route not found"