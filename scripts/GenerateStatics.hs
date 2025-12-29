#!/usr/bin/env runhaskell

import GenerateEmbed
import GenerateFeeds
import GenerateIcsGeojson
import GenerateUtils

main :: IO ()
main = do
  putStrLn "Starting static generation..."
  events <- fetchPublishedEvents
  putStrLn $ "Fetched " ++ show (length events) ++ " published events"

  generateEmbed events
  generateFeeds events
  generateIcsCalendar events
  generateGeojson events

  putStrLn $ "Completed static generation for " ++ show (length events) ++ " events"
