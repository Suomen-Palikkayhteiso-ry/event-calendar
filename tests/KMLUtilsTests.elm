module KMLUtilsTests exposing (..)

{-| Tests for KMLUtils
Reference: agents/stories/create-event.md (KML Import logic)
ADRs: ADR-0012
-}

import Expect
import KMLUtils
import Test exposing (..)


suite : Test
suite =
    describe "KMLUtils"
        [ describe "parseEventName"
            [ test "should parse event name with country and dates" <|
                \_ ->
                    let
                        result = KMLUtils.parseEventName "Event Title (FIN) 15-20 July"
                    in
                    Expect.equal result { title = "Event Title", country = Just "FIN", dates = Just "15-20 July" }
            , test "should parse event name with country only" <|
                \_ ->
                    let
                        result = KMLUtils.parseEventName "Event Title (FIN)"
                    in
                    Expect.equal result { title = "Event Title", country = Just "FIN", dates = Nothing }
            , test "should parse event name without country" <|
                \_ ->
                    let
                        result = KMLUtils.parseEventName "Event Title"
                    in
                    Expect.equal result { title = "Event Title", country = Nothing, dates = Nothing }
            ]
        , describe "parseDateString"
            [ test "should parse mid month date" <|
                \_ ->
                    let
                        result = KMLUtils.parseDateString "mid July" 2024
                    in
                    Expect.equal result { startDate = Just "2024-07-15", endDate = Just "2024-07-15" }
            , test "should parse in month date" <|
                \_ ->
                    let
                        result = KMLUtils.parseDateString "in July" 2024
                    in
                    Expect.equal result { startDate = Just "2024-07-01", endDate = Just "2024-07-31" }
            , test "should parse day range" <|
                \_ ->
                    let
                        result = KMLUtils.parseDateString "July 15-20" 2024
                    in
                    Expect.equal result { startDate = Just "2024-07-15", endDate = Just "2024-07-20" }
            , test "should parse single day" <|
                \_ ->
                    let
                        result = KMLUtils.parseDateString "July 15" 2024
                    in
                    Expect.equal result { startDate = Just "2024-07-15", endDate = Just "2024-07-15" }
            ]
        ]