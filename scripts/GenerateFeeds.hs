{-# LANGUAGE OverloadedStrings #-}

module GenerateFeeds where

import GenerateUtils
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL
import Data.Time
import Data.List (intercalate)

generateFeeds :: [Event] -> IO ()
generateFeeds events = do
  generateRss events
  generateAtom events
  generateJsonFeed events

generateRss :: [Event] -> IO ()
generateRss events = do
  let xml = TL.concat [
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<rss version=\"2.0\">\n",
        "<channel>\n",
        "<title>Kalenteri</title>\n",
        "<link>", TL.pack baseUrl, "</link>\n",
        "<description>Tapahtumakalenteri</description>\n",
        TL.concat $ map eventToRssItem events,
        "</channel>\n",
        "</rss>"
        ]
  writeStaticFile "kalenteri.rss" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict xml)

generateAtom :: [Event] -> IO ()
generateAtom events = do
  let xml = TL.concat [
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
        "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n",
        "<title>Kalenteri</title>\n",
        "<link href=\"", TL.pack baseUrl, "\"/>\n",
        "<updated>", TL.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ UTCTime (fromGregorian 2023 1 1) 0, "</updated>\n",
        "<author><name>Kalenteri</name></author>\n",
        TL.concat $ map eventToAtomItem events,
        "</feed>"
        ]
  writeStaticFile "kalenteri.atom" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict xml)

generateJsonFeed :: [Event] -> IO ()
generateJsonFeed events = do
  let json = TL.concat [
        "{\n",
        "\"version\": \"https://jsonfeed.org/version/1\",\n",
        "\"title\": \"Kalenteri\",\n",
        "\"home_page_url\": \"", TL.pack baseUrl, "\",\n",
        "\"feed_url\": \"", TL.pack baseUrl, "/kalenteri.json\",\n",
        "\"items\": [\n",
        TL.intercalate ",\n" $ map eventToJsonItem events,
        "\n]\n",
        "}"
        ]
  writeStaticFile "kalenteri.json" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict json)

eventToRssItem :: Event -> TL.Text
eventToRssItem event = TL.concat [
  "<item>\n",
  "<title>", TL.fromStrict $ title event, "</title>\n",
  "<link>", TL.pack baseUrl, "/event/", TL.fromStrict $ eventId event, "</link>\n",
  maybe "" (\d -> TL.concat ["<description>", TL.fromStrict d, "</description>\n"]) (description event),
  "</item>\n"
  ]

eventToAtomItem :: Event -> TL.Text
eventToAtomItem event = TL.concat [
  "<entry>\n",
  "<title>", TL.fromStrict $ title event, "</title>\n",
  "<link href=\"", TL.pack baseUrl, "/event/", TL.fromStrict $ eventId event, "\"/>\n",
  "<id>", TL.fromStrict $ eventId event, "</id>\n",
  "<updated>", TL.fromStrict $ created event, "</updated>\n",
  maybe "" (\d -> TL.concat ["<summary>", TL.fromStrict d, "</summary>\n"]) (description event),
  "</entry>\n"
  ]

eventToJsonItem :: Event -> TL.Text
eventToJsonItem event = TL.concat [
  "{\n",
  "\"id\": \"", TL.fromStrict $ eventId event, "\",\n",
  "\"url\": \"", TL.pack baseUrl, "/event/", TL.fromStrict $ eventId event, "\",\n",
  "\"title\": \"", TL.fromStrict $ title event, "\",\n",
  maybe "" (\d -> TL.concat ["\"content_text\": \"", TL.fromStrict d, "\",\n"]) (description event),
  "\"date_published\": \"", TL.fromStrict $ created event, "\"\n",
  "}"
  ]