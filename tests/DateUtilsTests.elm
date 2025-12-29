module DateUtilsTests exposing (..)

{-| Tests for DateUtils
Reference: agents/stories/view-calendar.md, agents/stories/view-event-details.md
ADRs: ADR-0012, ADR-0005
-}

import DateUtils
import Expect
import Test exposing (..)
import Time


suite : Test
suite =
    describe "DateUtils"
        [ describe "parseUTCDate"
            [ test "should parse UTC string with Z suffix" <|
                \_ ->
                    let
                        result =
                            DateUtils.parseUTCDate "2024-01-01T10:00:00Z"

                        expectedMillis =
                            1704103200000

                        -- approximate
                    in
                    Expect.equal (Time.posixToMillis result) expectedMillis
            , test "should parse UTC string without Z suffix" <|
                \_ ->
                    let
                        result =
                            DateUtils.parseUTCDate "2024-01-01T10:00:00"

                        expectedMillis =
                            1704103200000
                    in
                    Expect.equal (Time.posixToMillis result) expectedMillis
            ]
        , describe "formatDateInHelsinki"
            [ test "should format all-day date correctly" <|
                \_ ->
                    let
                        result =
                            DateUtils.formatDateInHelsinki "2024-01-01T10:00:00Z" True
                    in
                    Expect.equal result "1.1.2024"
            , test "should format timed date correctly" <|
                \_ ->
                    let
                        result =
                            DateUtils.formatDateInHelsinki "2024-01-01T10:30:00Z" False
                    in
                    Expect.equal result "1.1.2024 12:30"
            ]
        ]
