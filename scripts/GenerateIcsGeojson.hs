{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GenerateIcsGeojson where

import GenerateUtils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import Data.Time

data Feature = Feature
  { type_ :: T.Text
  , geometry :: Maybe Value
  , properties :: Value
  } deriving (Generic)

instance ToJSON Feature where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case "type_" -> "type"; x -> x }

data FeatureCollection = FeatureCollection
  { type_ :: T.Text
  , features :: [Feature]
  } deriving (Generic)

instance ToJSON FeatureCollection where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = \case "type_" -> "type"; x -> x }

generateIcsCalendar :: [Event] -> IO ()
generateIcsCalendar events = do
  let ics = TL.concat [
        "BEGIN:VCALENDAR\n",
        "VERSION:2.0\n",
        "PRODID:-//Kalenteri//Haskell//EN\n",
        TL.concat $ map eventToVEvent events,
        "END:VCALENDAR\n"
        ]
  writeStaticFile "kalenteri.ics" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict ics)

generateGeojson :: [Event] -> IO ()
generateGeojson events = do
  let features = map eventToFeature events
  let collection = FeatureCollection { type_ = "FeatureCollection", features = features }
  let json = encode collection
  writeStaticFile "kalenteri.geo.json" json

eventToVEvent :: Event -> TL.Text
eventToVEvent event = TL.concat [
  "BEGIN:VEVENT\n",
  "UID:", TL.fromStrict $ eventId event, "\n",
  "DTSTART:", TL.fromStrict $ startDate event, "\n",
  maybe "" (\e -> TL.concat ["DTEND:", TL.fromStrict e, "\n"]) (endDate event),
  "SUMMARY:", TL.fromStrict $ title event, "\n",
  maybe "" (\d -> TL.concat ["DESCRIPTION:", TL.fromStrict d, "\n"]) (description event),
  maybe "" (\l -> TL.concat ["LOCATION:", TL.fromStrict l, "\n"]) (location event),
  "END:VEVENT\n"
  ]

eventToFeature :: Event -> Feature
eventToFeature event = Feature
  { type_ = "Feature"
  , geometry = Nothing
  , properties = object
      [ "title" .= title event
      , "description" .= description event
      , "start_date" .= startDate event
      , "end_date" .= endDate event
      ]
  }