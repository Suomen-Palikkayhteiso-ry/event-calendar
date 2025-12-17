module DateUtils exposing (..)module DateUtils exposing (..)

import Time


-- Parse UTC ISO string to Posix


parseUTCDate : String -> Time.Posix
parseUTCDate utcString =
    -- Simple parsing, assuming ISO format
    case String.split "T" utcString of
        [ date, time ] ->
            case String.split "-" date of
                [ y, m, d ] ->
                    case String.split ":" (String.dropRight 1 time) of
                        [ h, min, s ] ->
                            Time.millisToPosix
                                ( (String.toInt y |> Maybe.withDefault 1970) * 365 * 24 * 60 * 60 * 1000
                                    + (String.toInt m |> Maybe.withDefault 1) * 30 * 24 * 60 * 60 * 1000
                                    + (String.toInt d |> Maybe.withDefault 1) * 24 * 60 * 60 * 1000
                                    + (String.toInt h |> Maybe.withDefault 0) * 60 * 60 * 1000
                                    + (String.toInt min |> Maybe.withDefault 0) * 60 * 1000
                                    + (String.toInt s |> Maybe.withDefault 0) * 1000
                                )

                        _ ->
                            Time.millisToPosix 0

                _ ->
                    Time.millisToPosix 0

        _ ->
            Time.millisToPosix 0


-- Format date in Helsinki timezone (approximate)


formatDateInHelsinki : String -> Bool -> String
formatDateInHelsinki utcString allDay =
    let
        posix = parseUTCDate utcString
        -- Approximate Helsinki time (UTC +2 or +3)
        helsinkiMillis = Time.posixToMillis posix + 2 * 60 * 60 * 1000 -- +2 hours
        helsinkiPosix = Time.millisToPosix helsinkiMillis
        year = Time.toYear Time.utc helsinkiPosix
        month = Time.toMonth Time.utc helsinkiPosix
        day = Time.toDay Time.utc helsinkiPosix
        dateStr =
            String.fromInt day
                ++ "."
                ++ String.fromInt (monthToInt month)
                ++ "."
                ++ String.fromInt year
    in
    if allDay then
        dateStr
    else
        let
            hour = Time.toHour Time.utc helsinkiPosix
            minute = Time.toMinute Time.utc helsinkiPosix
            timeStr =
                String.padLeft 2 '0' (String.fromInt hour)
                    ++ ":"
                    ++ String.padLeft 2 '0' (String.fromInt minute)
        in
        dateStr ++ " " ++ timeStr


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan -> 1
        Time.Feb -> 2
        Time.Mar -> 3
        Time.Apr -> 4
        Time.May -> 5
        Time.Jun -> 6
        Time.Jul -> 7
        Time.Aug -> 8
        Time.Sep -> 9
        Time.Oct -> 10
        Time.Nov -> 11
        Time.Dec -> 12


-- Convert local date to UTC


localDateToUTC : String -> String
localDateToUTC localDateString =
    -- Assume local is Helsinki, convert to UTC
    localDateString ++ "T00:00:00.000Z" -- placeholder


localDateTimeToUTC : String -> String
localDateTimeToUTC localDateTimeString =
    localDateTimeString ++ ":00.000Z" -- placeholder


utcToHelsinkiDateTimeLocal : String -> String
utcToHelsinkiDateTimeLocal utcString =
    let
        posix = parseUTCDate utcString
        helsinkiMillis = Time.posixToMillis posix + 2 * 60 * 60 * 1000
        helsinkiPosix = Time.millisToPosix helsinkiMillis
        year = Time.toYear Time.utc helsinkiPosix
        month = Time.toMonth Time.utc helsinkiPosix
        day = Time.toDay Time.utc helsinkiPosix
        hour = Time.toHour Time.utc helsinkiPosix
        minute = Time.toMinute Time.utc helsinkiPosix
    in
    String.fromInt year
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (monthToInt month))
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt day)
        ++ "T"
        ++ String.padLeft 2 '0' (String.fromInt hour)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt minute)


dateToHelsinkiDateString : Time.Posix -> String
dateToHelsinkiDateString posix =
    let
        helsinkiMillis = Time.posixToMillis posix + 2 * 60 * 60 * 1000
        helsinkiPosix = Time.millisToPosix helsinkiMillis
        year = Time.toYear Time.utc helsinkiPosix
        month = Time.toMonth Time.utc helsinkiPosix
        day = Time.toDay Time.utc helsinkiPosix
    in
    String.fromInt year
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (monthToInt month))
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt day)