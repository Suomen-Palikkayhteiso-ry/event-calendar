{-# LANGUAGE OverloadedStrings #-}

module GenerateIcsGeojson where

import GenerateUtils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL

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
  let json = TL.concat [
        "{\n",
        "\"type\": \"FeatureCollection\",\n",
        "\"features\": [\n",
        TL.intercalate ",\n" $ map eventToFeature events,
        "\n]\n",
        "}"
        ]
  writeStaticFile "kalenteri.geo.json" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict json)

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

eventToFeature :: Event -> TL.Text
eventToFeature event = TL.concat [
  "{\n",
  "\"type\": \"Feature\",\n",
  "\"geometry\": null,\n",
  "\"properties\": {\n",
  "\"title\": \"", TL.fromStrict $ title event, "\",\n",
  "\"description\": ", maybe "null" (\d -> "\"" <> TL.fromStrict d <> "\"") (description event), ",\n",
  "\"start_date\": \"", TL.fromStrict $ startDate event, "\",\n",
  "\"end_date\": ", maybe "null" (\e -> "\"" <> TL.fromStrict e <> "\"") (endDate event), "\n",
  "}\n",
  "}"
  ]