module KMLUtils exposing (..)

import DateUtils
import Dict exposing (Dict)


months : Dict String Int
months =
    Dict.fromList
        [ ( "january", 0 )
        , ( "jan", 0 )
        , ( "february", 1 )
        , ( "feb", 1 )
        , ( "march", 2 )
        , ( "mar", 2 )
        , ( "april", 3 )
        , ( "apr", 3 )
        , ( "may", 4 )
        , ( "june", 5 )
        , ( "jun", 5 )
        , ( "july", 6 )
        , ( "jul", 6 )
        , ( "august", 7 )
        , ( "aug", 7 )
        , ( "september", 8 )
        , ( "sep", 8 )
        , ( "october", 9 )
        , ( "oct", 9 )
        , ( "november", 10 )
        , ( "nov", 10 )
        , ( "december", 11 )
        , ( "dec", 11 )
        ]


countryMap : Dict String String
countryMap =
    Dict.fromList
        [ ( "LAT", "Latvia" )
        , ( "EST", "Estonia" )
        , ( "LIT", "Lithuania" )
        , ( "FIN", "Finland" )
        , ( "SWE", "Sweden" )
        , ( "NOR", "Norway" )
        , ( "DNK", "Denmark" )
        ]


type alias ParsedEventName =
    { title : String
    , country : Maybe String
    , dates : Maybe String
    }


parseEventName : String -> ParsedEventName
parseEventName name =
    let
        openParen = String.indexes "(" name
        closeParen = String.indexes ")" name
    in
    case ( List.head openParen, List.head closeParen ) of
        ( Just open, Just close ) ->
            if open < close then
                let
                    title = String.left open name |> String.trim
                    country = String.slice (open + 1) close name |> String.trim
                    dates = String.dropLeft (close + 1) name |> String.trim
                in
                { title = title
                , country = if String.isEmpty country then Nothing else Just country
                , dates = if String.isEmpty dates then Nothing else Just dates
                }
            else
                { title = name, country = Nothing, dates = Nothing }
        _ ->
            { title = name, country = Nothing, dates = Nothing }


parseDateString : String -> Int -> { startDate : Maybe String, endDate : Maybe String }
parseDateString dateStr year =
    let
        lower =
            String.toLower dateStr
    in
    if String.contains "mid" lower then
        case String.split " " lower |> List.filter (not << String.isEmpty) of
            _ :: monthName :: _ ->
                case Dict.get monthName months of
                    Just month ->
                        let
                            dateStr_ =
                                DateUtils.dateToString { year = year, month = month + 1, day = 15 }
                        in
                        { startDate = Just dateStr_, endDate = Just dateStr_ }

                    Nothing ->
                        { startDate = Nothing, endDate = Nothing }

            _ ->
                { startDate = Nothing, endDate = Nothing }

    else if String.startsWith "in " lower then
        case String.dropLeft 3 lower |> String.trim |> String.split " " |> List.head of
            Just monthName ->
                case Dict.get monthName months of
                    Just month ->
                        let
                            start =
                                DateUtils.dateToString { year = year, month = month + 1, day = 1 }

                            endDay =
                                case month of
                                    0 -> 31 -- Jan
                                    1 -> 28 -- Feb
                                    2 -> 31 -- Mar
                                    3 -> 30 -- Apr
                                    4 -> 31 -- May
                                    5 -> 30 -- Jun
                                    6 -> 31 -- Jul
                                    7 -> 31 -- Aug
                                    8 -> 30 -- Sep
                                    9 -> 31 -- Oct
                                    10 -> 30 -- Nov
                                    11 -> 31 -- Dec
                                    _ -> 30

                            end =
                                DateUtils.dateToString { year = year, month = month + 1, day = endDay }
                        in
                        { startDate = Just start, endDate = Just end }

                    Nothing ->
                        { startDate = Nothing, endDate = Nothing }

            Nothing ->
                { startDate = Nothing, endDate = Nothing }

    else
        case String.split " " dateStr |> List.filter (not << String.isEmpty) of
            monthName :: day1Str :: rest ->
                let
                    day1Parts = String.split "-" day1Str
                in
                case ( Dict.get (String.toLower monthName) months, List.head day1Parts |> Maybe.andThen String.toInt ) of
                    ( Just month, Just day1 ) ->
                        let
                            day2 =
                                case day1Parts of
                                    _ :: day2Str :: _ ->
                                        String.toInt day2Str |> Maybe.withDefault day1

                                    _ ->
                                        case rest of
                                            [ day2Str ] ->
                                                String.toInt day2Str |> Maybe.withDefault day1

                                            _ ->
                                                day1

                            start =
                                DateUtils.dateToString { year = year, month = month + 1, day = day1 }

                            end =
                                DateUtils.dateToString { year = year, month = month + 1, day = day2 }
                        in
                        { startDate = Just start, endDate = Just end }

                    _ ->
                        { startDate = Nothing, endDate = Nothing }

            _ ->
                { startDate = Nothing, endDate = Nothing }