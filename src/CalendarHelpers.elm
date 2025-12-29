module CalendarHelpers exposing (startOfDayMillis)

import DateHelpers
import Time


startOfDayMillis : Time.Posix -> Int
startOfDayMillis posix =
    let
        year =
            Time.toYear Time.utc posix

        month =
            Time.toMonth Time.utc posix

        day =
            Time.toDay Time.utc posix
    in
    Time.posixToMillis (DateHelpers.dateToPosix year month day)
