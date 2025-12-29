{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module GenerateUtils where

import Data.Aeson
import Data.Time
import Data.Time.Clock
import Data.Time.LocalTime
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import Data.List (sortOn)
import Control.Monad (forM_, when)
import GHC.Generics

-- PocketBase URL
pocketbaseUrl :: String
pocketbaseUrl = "https://data.suomenpalikkayhteiso.fi"

-- Day abbreviations
dayAbbr :: [String]
dayAbbr = ["su", "ma", "ti", "ke", "to", "pe", "la"]

-- PocketBase API response
data PBResponse a = PBResponse
  { page :: Int
  , perPage :: Int
  , totalItems :: Int
  , totalPages :: Int
  , items :: [a]
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (PBResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- Event data type
data Event = Event
  { eventId :: T.Text
  , collectionId :: T.Text
  , collectionName :: T.Text
  , created :: T.Text
  , updated :: T.Text
  , startDate :: T.Text
  , endDate :: Maybe T.Text
  , allDay :: Bool
  , title :: T.Text
  , description :: Maybe T.Text
  , location :: Maybe T.Text
  , state :: T.Text
  , image :: Maybe T.Text
  } deriving (Show, Generic)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> Event
    <$> v .: "id"
    <*> v .: "collectionId"
    <*> v .: "collectionName"
    <*> v .: "created"
    <*> v .: "updated"
    <*> v .: "start_date"
    <*> v .:? "end_date"
    <*> v .: "all_day"
    <*> v .: "title"
    <*> v .:? "description"
    <*> v .:? "location"
    <*> v .: "state"
    <*> v .:? "image"

instance ToJSON Event where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case
    "eventId" -> "id"
    "collectionId" -> "collectionId"
    "collectionName" -> "collectionName"
    "startDate" -> "start_date"
    "endDate" -> "end_date"
    "allDay" -> "all_day"
    other -> other
  }

-- Convert UTC string to Helsinki time
toHelsinkiTime :: T.Text -> Maybe LocalTime
toHelsinkiTime utcStr = do
  let utcStr' = if T.last utcStr == 'Z' then T.init utcStr else utcStr
  utcTime <- parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack utcStr')
  let utcTime' = UTCTime (utctDay utcTime) (utctDayTime utcTime)
  let helsinkiOffset = 7200  -- 2 hours in seconds
  let helsinkiTime = addUTCTime (fromIntegral helsinkiOffset) utcTime'
  return $ utcToLocalTime (hoursToTimeZone 2) helsinkiTime

-- Format date in Helsinki
formatDateInHelsinki :: T.Text -> Bool -> String
formatDateInHelsinki utcStr allDay = case toHelsinkiTime utcStr of
  Nothing -> ""
  Just localTime -> 
    let day = show $ localDay localTime
        month = show $ (\(y,m,d) -> m) $ toGregorian $ localDay localTime
        dateStr = day ++ "." ++ month ++ "."
    in if allDay
       then dateStr
       else let timeOfDay = localTimeOfDay localTime
                hours = show $ todHour timeOfDay
                mins = show $ todMin timeOfDay
                timeStr = hours ++ "." ++ mins
            in dateStr ++ " " ++ timeStr

-- Fetch published events
fetchPublishedEvents :: IO [Event]
fetchPublishedEvents = do
  manager <- newManager tlsManagerSettings
  yesterday <- getCurrentTime >>= \now -> return $ addDays (-1) (utctDay now)
  let yesterdayStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (UTCTime yesterday 0)
  let url = pocketbaseUrl ++ "/api/collections/events/records?filter=state%20%3D%20%22published%22%20%26%26%20%28end_date%20%3E%20%22" ++ yesterdayStr ++ "%22%20%7C%7C%20start_date%20%3E%20%22" ++ yesterdayStr ++ "%22%29&sort=start_date"
  request <- parseRequest url
  response <- httpLbs request manager
  let body = responseBody response
  case eitherDecode body of
    Left err -> error $ "Failed to parse JSON: " ++ err
    Right response -> return $ items response

-- Write static file
writeStaticFile :: FilePath -> BSL.ByteString -> IO ()
writeStaticFile relativePath content = do
  let staticPath = "static" </> relativePath
  createDirectoryIfMissing True (takeDirectory staticPath)
  BSL.writeFile staticPath content

  -- Also write to build if exists
  buildExists <- doesDirectoryExist "build"
  when buildExists $ do
    let buildPath = "build" </> relativePath
    createDirectoryIfMissing True (takeDirectory buildPath)
    BSL.writeFile buildPath content