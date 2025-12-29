{-# LANGUAGE OverloadedStrings #-}

module GenerateFeeds where

import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import GenerateUtils
import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.Feed.Export
import Text.Feed.Types
import Text.RSS.Syntax

generateFeeds :: [Event] -> IO ()
generateFeeds events = do
  generateRss events
  generateAtom events
  generateJsonFeed events

generateRss :: [Event] -> IO ()
generateRss events = do
  let rssFeed =
        RSSFeed
          { rssVersion = "2.0",
            rssChannel =
              RSSChannel
                { rssTitle = "Kalenteri",
                  rssLink = T.pack baseUrl,
                  rssDescription = "Tapahtumakalenteri",
                  rssItems = map eventToRssItem events,
                  rssLanguage = Nothing,
                  rssCopyright = Nothing,
                  rssEditor = Nothing,
                  rssWebMaster = Nothing,
                  rssPubDate = Nothing,
                  rssLastBuildDate = Nothing,
                  rssCategories = [],
                  rssGenerator = Nothing,
                  rssDocs = Nothing,
                  rssCloud = Nothing,
                  rssTTL = Nothing,
                  rssImage = Nothing,
                  rssRating = Nothing,
                  rssTextInput = Nothing,
                  rssSkipHours = [],
                  rssSkipDays = [],
                  rssExtensions = []
                },
            rssAttrs = [],
            rssOther = []
          }
  let xml = rssToXML rssFeed
  writeStaticFile "kalenteri.rss" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict xml)

generateAtom :: [Event] -> IO ()
generateAtom events = do
  let atomFeed =
        AtomFeed
          { atomFeedTitle = TextString "Kalenteri",
            atomFeedUpdated = UTCTime (fromGregorian 2023 1 1) 0,
            atomFeedAuthors = [Person {personName = "Kalenteri", personURI = Nothing, personEmail = Nothing}],
            atomFeedId = T.pack baseUrl,
            atomFeedLinks = [Link {linkHref = T.pack baseUrl, linkRel = Just "alternate", linkType = Nothing, linkHrefLang = Nothing, linkTitle = Nothing, linkLength = Nothing}],
            atomFeedCategories = [],
            atomFeedContributors = [],
            atomFeedRights = Nothing,
            atomFeedEntries = map eventToAtomEntry events,
            atomFeedAttrs = [],
            atomFeedOther = [],
            atomFeedGenerator = Nothing,
            atomFeedIcon = Nothing,
            atomFeedLogo = Nothing,
            atomFeedSubtitle = Nothing
          }
  let xml = atomFeedToXML atomFeed
  writeStaticFile "kalenteri.atom" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict xml)

generateJsonFeed :: [Event] -> IO ()
generateJsonFeed events = do
  let json =
        TL.concat
          [ "{\n",
            "\"version\": \"https://jsonfeed.org/version/1\",\n",
            "\"title\": \"Kalenteri\",\n",
            "\"home_page_url\": \"",
            TL.pack baseUrl,
            "\",\n",
            "\"feed_url\": \"",
            TL.pack baseUrl,
            "/kalenteri.json\",\n",
            "\"items\": [\n",
            TL.intercalate ",\n" $ map eventToJsonItem events,
            "\n]\n",
            "}"
          ]
  writeStaticFile "kalenteri.json" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict json)

eventToRssItem :: Event -> RSSItem
eventToRssItem event =
  RSSItem
    { rssItemTitle = Just $ title event,
      rssItemLink = Just $ T.pack baseUrl <> "/event/" <> eventId event,
      rssItemDescription = description event,
      rssItemAuthor = Nothing,
      rssItemCategories = [],
      rssItemComments = Nothing,
      rssItemEnclosure = Nothing,
      rssItemGuid = Just $ RSSGuid (T.pack baseUrl <> "/event/" <> eventId event) False,
      rssItemPubDate = Nothing,
      rssItemSource = Nothing,
      rssItemAttrs = [],
      rssItemOther = []
    }

eventToAtomEntry :: Event -> Entry
eventToAtomEntry event =
  Entry
    { entryId = eventId event,
      entryTitle = TextString $ title event,
      entryUpdated = UTCTime (fromGregorian 2023 1 1) 0, -- TODO: parse created
      entryAuthors = [],
      entryCategories = [],
      entryContent = fmap TextString (description event),
      entryContributor = [],
      entryLinks = [Link {linkHref = T.pack baseUrl <> "/event/" <> eventId event, linkRel = Just "alternate", linkType = Nothing, linkHrefLang = Nothing, linkTitle = Nothing, linkLength = Nothing}],
      entryPublished = Nothing,
      entryRights = Nothing,
      entrySource = Nothing,
      entrySummary = Nothing,
      entryInReplyTo = Nothing,
      entryInReplyToRefs = [],
      entryAttrs = [],
      entryOther = []
    }

eventToJsonItem :: Event -> TL.Text
eventToJsonItem event =
  TL.concat
    [ "{\n",
      "\"id\": \"",
      TL.fromStrict $ eventId event,
      "\",\n",
      "\"url\": \"",
      TL.pack baseUrl,
      "/event/",
      TL.fromStrict $ eventId event,
      "\",\n",
      "\"title\": \"",
      TL.fromStrict $ title event,
      "\",\n",
      maybe "" (\d -> TL.concat ["\"content_text\": \"", TL.fromStrict d, "\",\n"]) (description event),
      "\"date_published\": \"",
      TL.fromStrict $ created event,
      "\"\n",
      "}"
    ]
