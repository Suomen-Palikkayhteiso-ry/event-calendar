module EventUtils exposing (..)

import DateUtils
import Time
import Types exposing (Event, EventFormData, EventState(..), Point, DisplayEvent)


eventToFormData : Event -> EventFormData
eventToFormData event =
    { title = event.title
    , start_date = event.startDate
    , end_date = event.endDate |> Maybe.withDefault event.startDate
    , all_day = event.allDay
    , location = event.location |> Maybe.withDefault ""
    , description = event.description |> Maybe.withDefault ""
    , url = event.url |> Maybe.withDefault ""
    , image = Nothing
    , image_description = event.imageDescription |> Maybe.withDefault ""
    , state = event.state
    , point = event.point
    }


formatEventForDisplay : Event -> DisplayEvent
formatEventForDisplay event =
    let
        startPosix = DateUtils.parseUTCDate event.startDate
        -- Approximate Helsinki
        helsinkiMillis = Time.posixToMillis startPosix + 2 * 60 * 60 * 1000
        helsinkiPosix = Time.millisToPosix helsinkiMillis
        year = Time.toYear Time.utc helsinkiPosix
        month = Time.toMonth Time.utc helsinkiPosix
        day = Time.toDay Time.utc helsinkiPosix
        displayDate = String.fromInt day ++ "." ++ String.fromInt (DateUtils.monthToInt month) ++ "." ++ String.fromInt year
        displayTime =
            if event.allDay then
                "Koko päivä"
            else
                let
                    hour = Time.toHour Time.utc helsinkiPosix
                    minute = Time.toMinute Time.utc helsinkiPosix
                in
                String.padLeft 2 '0' (String.fromInt hour) ++ ":" ++ String.padLeft 2 '0' (String.fromInt minute)
    in
    { event = event, displayDate = displayDate, displayTime = displayTime }


isEventOngoing : Event -> Bool
isEventOngoing event =
    let
        now = Time.millisToPosix 0 -- placeholder for current time
        start = DateUtils.parseUTCDate event.startDate
        end = event.endDate |> Maybe.map DateUtils.parseUTCDate |> Maybe.withDefault start
    in
    Time.posixToMillis now >= Time.posixToMillis start && Time.posixToMillis now <= Time.posixToMillis end


isEventUpcoming : Event -> Bool
isEventUpcoming event =
    let
        now = Time.millisToPosix 0
        start = DateUtils.parseUTCDate event.startDate
    in
    Time.posixToMillis start > Time.posixToMillis now


isEventPast : Event -> Bool
isEventPast event =
    let
        now = Time.millisToPosix 0
        end = event.endDate |> Maybe.map DateUtils.parseUTCDate |> Maybe.withDefault (DateUtils.parseUTCDate event.startDate)
    in
    Time.posixToMillis end < Time.posixToMillis now