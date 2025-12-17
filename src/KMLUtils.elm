module KMLUtils exposing (..)module KMLUtils exposing (..)

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
    case String.split "(" name |> List.map String.trim of
        title :: rest ->
            case String.join " " rest |> String.split ")" of
                countryAndDates :: _ ->
                    let
                        parts =
                            String.split " " countryAndDates |> List.filter (not << String.isEmpty)
                    in
                    case parts of
                        country :: dates ->
                            { title = title
                            , country = Just country
                            , dates = Just (String.join " " dates)
                            }

                        _ ->
                            { title = title
                            , country = Just countryAndDates
                            , dates = Nothing
                            }

                _ ->
                    { title = title
                    , country = Nothing
                    , dates = Nothing
                    }

        _ ->
            { title = name
            , country = Nothing
            , dates = Nothing
            }


parseDateString : String -> Int -> { startDate : Maybe String, endDate : Maybe String }
parseDateString dateStr year =
    let
        lower =
            String.toLowerCase dateStr
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

                            end =
                                DateUtils.dateToString { year = year, month = month + 2, day = 0 }
                        in
                        { startDate = Just start, endDate = Just end }

                    Nothing ->
                        { startDate = Nothing, endDate = Nothing }

            Nothing ->
                { startDate = Nothing, endDate = Nothing }

    else
        case String.split " " dateStr |> List.filter (not << String.isEmpty) of
            monthName :: day1Str :: rest ->
                case ( Dict.get (String.toLowerCase monthName) months, String.toInt day1Str ) of
                    ( Just month, Ok day1 ) ->
                        let
                            day2 =
                                case rest of
                                    [ day2Str ] ->
                                        String.toInt day2Str |> Result.withDefault day1

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