module DateUtils exposing (..)

import Time


-- Parse UTC ISO string to Posix


parseUTCDate : String -> Time.Posix
parseUTCDate utcString =
    -- Parse ISO format like 2024-01-01T10:00:00Z
    case String.split "T" utcString of
        [ date, timeWithZ ] ->
            let
                time = String.dropRight 1 timeWithZ -- remove Z
            in
            case String.split "-" date of
                [ yStr, mStr, dStr ] ->
                    case String.split ":" time of
                        [ hStr, minStr, sStr ] ->
                            case String.toInt yStr of
                                Just y ->
                                    case String.toInt mStr of
                                        Just m ->
                                            case String.toInt dStr of
                                                Just d ->
                                                    case String.toInt hStr of
                                                        Just h ->
                                                            case String.toInt minStr of
                                                                Just min ->
                                                                    case String.toInt sStr of
                                                                        Just s ->
                                                                            let
                                                                                days = daysSinceEpoch y m d
                                                                                millis = days * 24 * 60 * 60 * 1000 + h * 60 * 60 * 1000 + min * 60 * 1000 + s * 1000
                                                                            in
                                                                            Time.millisToPosix millis

                                                                        _ ->
                                                                            Time.millisToPosix 0

                                                                _ ->
                                                                    Time.millisToPosix 0

                                                        _ ->
                                                            Time.millisToPosix 0

                                                _ ->
                                                    Time.millisToPosix 0

                                        _ ->
                                            Time.millisToPosix 0

                                _ ->
                                    Time.millisToPosix 0

                        _ ->
                            Time.millisToPosix 0

                _ ->
                    Time.millisToPosix 0

        _ ->
            Time.millisToPosix 0


daysSinceEpoch : Int -> Int -> Int -> Int
daysSinceEpoch year month day =
    let
        a = (14 - month) // 12
        y = year + 4800 - a
        m = month + 12 * a - 3
        jd = day + (153 * m + 2) // 5 + 365 * y + y // 4 - y // 100 + y // 400 - 32045
    in
    jd - 2440588 -- days since 1970-01-01


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