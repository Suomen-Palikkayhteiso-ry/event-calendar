module CalendarTests exposing (..)

{-| Tests for Calendar
Reference: agents/stories/view-calendar.md
ADRs: ADR-0012, ADR-0007
-}

import Calendar
import Expect
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Calendar"
        [ describe "isLeapYear"
            [ test "should return true for leap years divisible by 4" <|
                \_ ->
                    Expect.equal (Calendar.isLeapYear 2024) True
            , test "should return false for years not divisible by 4" <|
                \_ ->
                    Expect.equal (Calendar.isLeapYear 2023) False
            , test "should return false for century years not divisible by 400" <|
                \_ ->
                    Expect.equal (Calendar.isLeapYear 1900) False
            , test "should return true for century years divisible by 400" <|
                \_ ->
                    Expect.equal (Calendar.isLeapYear 2000) True
            ]
        , describe "monthToInt"
            [ test "should convert January to 0" <|
                \_ ->
                    Expect.equal (Calendar.monthToInt Time.Jan) 0
            , test "should convert December to 11" <|
                \_ ->
                    Expect.equal (Calendar.monthToInt Time.Dec) 11
            ]
        , describe "intToMonth"
            [ test "should convert 0 to January" <|
                \_ ->
                    Expect.equal (Calendar.intToMonth 0) Time.Jan
            , test "should convert 11 to December" <|
                \_ ->
                    Expect.equal (Calendar.intToMonth 11) Time.Dec
            ]
        , describe "daysInMonth"
            [ test "should return 31 for January" <|
                \_ ->
                    Expect.equal (Calendar.daysInMonth 2024 Time.Jan) 31
            , test "should return 28 for February in non-leap year" <|
                \_ ->
                    Expect.equal (Calendar.daysInMonth 2023 Time.Feb) 28
            , test "should return 29 for February in leap year" <|
                \_ ->
                    Expect.equal (Calendar.daysInMonth 2024 Time.Feb) 29
            ]
        , describe "addMonths"
            [ test "should add one month to January 1st" <|
                \_ ->
                    let
                        jan1 = Calendar.dateToPosix 2024 Time.Jan 1
                        feb1 = Calendar.addMonths 1 jan1
                        resultYear = Time.toYear Time.utc feb1
                        resultMonth = Time.toMonth Time.utc feb1
                        resultDay = Time.toDay Time.utc feb1
                    in
                    Expect.all
                        [ \() -> Expect.equal resultYear 2024
                        , \() -> Expect.equal resultMonth Time.Feb
                        , \() -> Expect.equal resultDay 1
                        ]
                        ()
            , test "should handle year rollover" <|
                \_ ->
                    let
                        dec1 = Calendar.dateToPosix 2023 Time.Dec 1
                        jan1 = Calendar.addMonths 1 dec1
                        resultYear = Time.toYear Time.utc jan1
                        resultMonth = Time.toMonth Time.utc jan1
                    in
                    Expect.all
                        [ \() -> Expect.equal resultYear 2024
                        , \() -> Expect.equal resultMonth Time.Jan
                        ]
                        ()
            ]
        ]