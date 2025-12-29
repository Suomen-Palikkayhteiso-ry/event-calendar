module DateHelpers exposing (addMonths, dateToPosix, daysInMonth, intToMonth, isLeapYear, monthToInt)

import Time


addMonths : Int -> Time.Posix -> Time.Posix
addMonths months posix =
    let
        year =
            Time.toYear Time.utc posix

        month =
            Time.toMonth Time.utc posix

        day =
            Time.toDay Time.utc posix

        totalMonths =
            monthToInt month + months

        newYear =
            year + (totalMonths // 12)

        newMonthInt =
            modBy 12 totalMonths

        newMonth =
            intToMonth newMonthInt

        daysInNewMonth =
            daysInMonth newYear newMonth

        newDay =
            min day daysInNewMonth
    in
    dateToPosix newYear newMonth newDay


dateToPosix : Int -> Time.Month -> Int -> Time.Posix
dateToPosix year month day =
    -- Approximate calculation, ignoring time zones and leap seconds
    let
        daysSinceEpoch =
            daysSince1970 year month day

        millis =
            daysSinceEpoch * 24 * 60 * 60 * 1000
    in
    Time.millisToPosix millis


daysSince1970 : Int -> Time.Month -> Int -> Int
daysSince1970 year month day =
    let
        years =
            year - 1970

        leapYears =
            (years // 4) - (years // 100) + (years // 400)

        daysInYears =
            years * 365 + leapYears

        daysInMonths =
            daysInMonthsBefore month
                + (if monthToInt month > 1 && isLeapYear year then
                    1

                   else
                    0
                  )
    in
    daysInYears + daysInMonths + day - 1


daysInMonthsBefore : Time.Month -> Int
daysInMonthsBefore month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            31

        Time.Mar ->
            59

        Time.Apr ->
            90

        Time.May ->
            120

        Time.Jun ->
            151

        Time.Jul ->
            181

        Time.Aug ->
            212

        Time.Sep ->
            243

        Time.Oct ->
            273

        Time.Nov ->
            304

        Time.Dec ->
            334


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            1

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            11


intToMonth : Int -> Time.Month
intToMonth int =
    case int of
        0 ->
            Time.Jan

        1 ->
            Time.Feb

        2 ->
            Time.Mar

        3 ->
            Time.Apr

        4 ->
            Time.May

        5 ->
            Time.Jun

        6 ->
            Time.Jul

        7 ->
            Time.Aug

        8 ->
            Time.Sep

        9 ->
            Time.Oct

        10 ->
            Time.Nov

        _ ->
            Time.Dec


daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    case month of
        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Apr ->
            30

        Time.Jun ->
            30

        Time.Sep ->
            30

        Time.Nov ->
            30

        _ ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0) && (modBy 100 year /= 0 || modBy 400 year == 0)
