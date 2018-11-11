{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Location.API where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , defaultOptions
                                                , genericToEncoding
                                                , toEncoding
                                                )
import           Data.SecureMem
import           Data.Text.Lazy                 ( pack
                                                , Text
                                                )
import qualified Database.SQLite.Simple        as SQLite
import           GHC.Generics
import qualified Location.Core                 as Core
import qualified Location.DB                   as DB
import qualified System.Posix.Env.ByteString   as Env
import           Web.Scotty

contentType :: Text
contentType = "Content-Type"

contentHTML :: Text
contentHTML = "text/html"

contentJSON :: Text
contentJSON = "application/json"

contentText :: Text
contentText = "text/plain"

data Response = Response
  { success      :: Bool
  , errorMessage :: String
  , coordinates  :: Maybe Core.Coordinates
  } deriving (Generic, Read, Show)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

getAuthPassword :: IO SecureMem
getAuthPassword =
  Env.getEnvDefault "WHEREAMI_PASS" "password"
    >>= return
    .   secureMemFromByteString

getCoordinates :: SQLite.Connection -> ActionM ()
getCoordinates conn = do
  acceptType <- header "Accept"
  case acceptType of
    Just ctype -> case ctype of
      "application/json" -> getCoordinatesJSON conn
      "text/plain"       -> getCoordinatesText conn
      _                  -> getCoordinatesHTML conn
    Nothing -> getCoordinatesHTML conn

getCoordinatesJSON :: SQLite.Connection -> ActionM ()
getCoordinatesJSON conn = do
  liftAndCatchIO $ putStrLn "getCoordinatesJSON"
  coordinates <- liftAndCatchIO $ DB.getCoordinates conn
  json $ Response True "" (Just coordinates)

getCoordinatesText :: SQLite.Connection -> ActionM ()
getCoordinatesText conn = do
  liftAndCatchIO $ putStrLn "getCoordinatesText"
  coordinates <- liftAndCatchIO $ DB.getCoordinates conn
  text . pack $ show coordinates

getCoordinatesHTML :: SQLite.Connection -> ActionM ()
getCoordinatesHTML conn = do
  liftAndCatchIO $ putStrLn "getCoordinatesHTML"
  coordinates <- liftAndCatchIO $ DB.getCoordinates conn
  file "page.html"
    -- build page

postCoordinates :: SQLite.Connection -> ActionM ()
postCoordinates conn = do
  coordinates <- jsonData :: ActionM Core.Coordinates
  liftAndCatchIO $ DB.storeCoordinates conn coordinates
  json $ Response True "" (Just coordinates)

notFound :: ActionM ()
notFound = json $ Response False "route not found" Nothing

staticPage :: FilePath -> ActionM ()
staticPage path = setHeader contentType contentHTML >> (file path)
