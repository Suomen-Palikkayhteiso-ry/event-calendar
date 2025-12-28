{-# LANGUAGE OverloadedStrings #-}

module GenerateEmbed where

import GenerateUtils
import Data.Time
import Data.Time.LocalTime
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.List (groupBy, sortOn)
import Data.Function (on)
import Data.Monoid ((<>))

baseUrl :: String
baseUrl = "https://kalenteri.suomenpalikkayhteiso.fi"

monthNames :: [String]
monthNames = [
  "Tammikuu", "Helmikuu", "Maaliskuu", "Huhtikuu", "Toukokuu",
  "Kesäkuu", "Heinäkuu", "Elokuu", "Syyskuu", "Lokakuu",
  "Marraskuu", "Joulukuu"
]

groupEventsByMonth :: [Event] -> [(Integer, Int, [Event])]
groupEventsByMonth events = 
  let sortedEvents = sortOn (localDay . fromJust . toHelsinkiTime . startDate) events
      grouped = groupBy ((==) `on` (\e -> let lt = fromJust $ toHelsinkiTime $ startDate e
                                             (y,m,_) = toGregorian $ localDay lt
                                         in (y,m))) sortedEvents
  in map (\es -> let lt = fromJust $ toHelsinkiTime $ startDate $ head es
                     (y,m,_) = toGregorian $ localDay lt
                 in (y, m, es)) grouped

generateQrCodeDataUrl :: String -> IO T.Text
generateQrCodeDataUrl url = do
  let qr = encodeText (defaultQRCodeOptions L) [] url
  case qr of
    Left err -> error $ "QR code error: " ++ show err
    Right qrCode -> do
      img <- toPngDataUrl qrCode
      return $ T.pack img

generateEmbed :: [Event] -> IO ()
generateEmbed events = do
  let groups = groupEventsByMonth events
  html <- generateHtml groups
  writeStaticFile "kalenteri.html" (BSL.fromStrict $ encodeUtf8 $ TL.toStrict html)

generateHtml :: [(Integer, Int, [Event])] -> IO TL.Text
generateHtml groups = do
  let builder = TB.fromText "<!DOCTYPE html>\n<html lang=\"fi\">\n<head>\n<meta charset=\"UTF-8\">\n<title>Kalenteri</title>\n</head>\n<body>\n"
  sections <- mapM (\(year, month, es) -> generateMonthSection year month es) groups
  let sectionsBuilder = mconcat sections
      builder' = builder <> sectionsBuilder <> TB.fromText "</body>\n</html>"
  return $ TB.toLazyText builder'

generateMonthSection :: Integer -> Int -> [Event] -> IO TB.Builder
generateMonthSection year month events = do
  let monthName = monthNames !! (month - 1)
      header = TB.fromText $ "<h2>" ++ show year ++ " " ++ monthName ++ "</h2>\n<ul>\n"
  items <- mapM generateEventItem events
  let itemsBuilder = mconcat items
      footer = TB.fromText "</ul>\n"
  return $ header <> itemsBuilder <> footer

generateEventItem :: Event -> IO TB.Builder
generateEventItem event = do
  -- For now, skip QR code
  let qr = TB.empty
  let title = TB.fromText $ "<li><strong>" ++ T.unpack (title event) ++ "</strong><br>"
      date = TB.fromText $ formatEventDisplayDate event ++ "<br>"
      desc = case description event of
               Just d -> TB.fromText $ T.unpack d ++ "<br>"
               Nothing -> TB.empty
      loc = case location event of
              Just l -> TB.fromText $ "Paikka: " ++ T.unpack l ++ "<br>"
              Nothing -> TB.empty
      end = TB.fromText "</li>\n"
  return $ title <> date <> qr <> desc <> loc <> end

formatEventDisplayDate :: Event -> String
formatEventDisplayDate event = 
  let startFormatted = formatDateInHelsinki (startDate event) (allDay event)
      dateStr = startFormatted
  in case endDate event of
       Nothing -> dateStr
       Just end -> let endFormatted = formatDateInHelsinki end (allDay event)
                   in if startFormatted == endFormatted
                      then dateStr
                      else dateStr ++ "–" ++ endFormatted