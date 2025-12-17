module EventUtilsTests exposing (..)

import DateUtils
import EventUtils
import Expect
import Test exposing (..)
import Time
import Types exposing (Event, EventFormData, EventState(..), Point, DisplayEvent)


suite : Test
suite =
    describe "EventUtils"
        [ describe "eventToFormData"
            [ test "should convert Event to EventFormData" <|
                \_ ->
                    let
                        event : Event
                        event =
                            { id = "test-id"
                            , title = "Test Event"
                            , description = Just "Test description"
                            , location = Just "Helsinki"
                            , startDate = "2024-01-01T10:00:00Z"
                            , endDate = Just "2024-01-01T12:00:00Z"
                            , allDay = False
                            , url = Just "https://example.com"
                            , image = Just "test-image.jpg"
                            , imageDescription = Just "Test image"
                            , point = Just { lat = 60.1699, lon = 24.9384 }
                            , state = Published
                            , created = "2024-01-01T09:00:00Z"
                            , updated = "2024-01-01T09:00:00Z"
                            }

                        formData = EventUtils.eventToFormData event
                    in
                    Expect.all
                        [ \fd -> Expect.equal fd.title "Test Event"
                        , \fd -> Expect.equal fd.description "Test description"
                        , \fd -> Expect.equal fd.location "Helsinki"
                        , \fd -> Expect.equal fd.start_date "2024-01-01T10:00:00Z"
                        , \fd -> Expect.equal fd.end_date "2024-01-01T12:00:00Z"
                        , \fd -> Expect.equal fd.all_day False
                        , \fd -> Expect.equal fd.url "https://example.com"
                        , \fd -> Expect.equal fd.image Nothing
                        , \fd -> Expect.equal fd.image_description "Test image"
                        , \fd -> Expect.equal fd.state Published
                        , \fd -> Expect.equal fd.point (Just { lat = 60.1699, lon = 24.9384 })
                        ]
                        formData
            , test "should handle missing optional fields" <|
                \_ ->
                    let
                        event : Event
                        event =
                            { id = "test-id"
                            , title = "Test Event"
                            , description = Nothing
                            , location = Nothing
                            , startDate = "2024-01-01T10:00:00Z"
                            , endDate = Nothing
                            , allDay = True
                            , url = Nothing
                            , image = Nothing
                            , imageDescription = Nothing
                            , point = Nothing
                            , state = Draft
                            , created = "2024-01-01T09:00:00Z"
                            , updated = "2024-01-01T09:00:00Z"
                            }

                        formData = EventUtils.eventToFormData event
                    in
                    Expect.all
                        [ \fd -> Expect.equal fd.title "Test Event"
                        , \fd -> Expect.equal fd.description ""
                        , \fd -> Expect.equal fd.location ""
                        , \fd -> Expect.equal fd.start_date "2024-01-01T10:00:00Z"
                        , \fd -> Expect.equal fd.end_date "2024-01-01T10:00:00Z"
                        , \fd -> Expect.equal fd.all_day True
                        , \fd -> Expect.equal fd.url ""
                        , \fd -> Expect.equal fd.image Nothing
                        , \fd -> Expect.equal fd.image_description ""
                        , \fd -> Expect.equal fd.state Draft
                        , \fd -> Expect.equal fd.point Nothing
                        ]
                        formData
            ]
        , describe "formatEventForDisplay"
            [ test "should format event for display" <|
                \_ ->
                    let
                        event : Event
                        event =
                            { id = "test-id"
                            , title = "Test Event"
                            , description = Just "Test description"
                            , location = Just "Helsinki"
                            , startDate = "2024-01-01T10:00:00Z"
                            , endDate = Just "2024-01-01T12:00:00Z"
                            , allDay = False
                            , url = Just "https://example.com"
                            , image = Just "test-image.jpg"
                            , imageDescription = Just "Test image"
                            , point = Just { lat = 60.1699, lon = 24.9384 }
                            , state = Published
                            , created = "2024-01-01T09:00:00Z"
                            , updated = "2024-01-01T09:00:00Z"
                            }

                        formatted = EventUtils.formatEventForDisplay event
                    in
                    Expect.all
                        [ \e -> Expect.equal e.displayDate "1.1.2024"
                        , \e -> Expect.equal e.displayTime "12:00"
                        ]
                        formatted
            ]
        , describe "isEventOngoing"
            [ test "should return true if event is ongoing" <|
                \_ ->
                    let
                        event : Event
                        event =
                            { id = "test-id"
                            , title = "Test Event"
                            , description = Just "Test description"
                            , location = Just "Helsinki"
                            , startDate = "2024-01-01T10:00:00Z"
                            , endDate = Just "2024-01-01T12:00:00Z"
                            , allDay = False
                            , url = Just "https://example.com"
                            , image = Just "test-image.jpg"
                            , imageDescription = Just "Test image"
                            , point = Just { lat = 60.1699, lon = 24.9384 }
                            , state = Published
                            , created = "2024-01-01T09:00:00Z"
                            , updated = "2024-01-01T09:00:00Z"
                            }

                        now = Time.millisToPosix (Time.posixToMillis (DateUtils.parseUTCDate "2024-01-01T11:00:00Z"))
                    in
                    Expect.equal (EventUtils.isEventOngoing now event) True
            , test "should return false if event is in the past" <|
                \_ ->
                    let
                        event = 
                            { id = "test-id"
                            , title = "Test Event"
                            , description = Just "Test description"
                            , location = Just "Helsinki"
                            , startDate = "2024-01-01T10:00:00Z"
                            , endDate = Just "2024-01-01T12:00:00Z"
                            , allDay = False
                            , url = Just "https://example.com"
                            , image = Just "test-image.jpg"
                            , imageDescription = Just "Test image"
                            , point = Just { lat = 60.1699, lon = 24.9384 }
                            , state = Published
                            , created = "2024-01-01T09:00:00Z"
                            , updated = "2024-01-01T09:00:00Z"
                            }

                        now = Time.millisToPosix (Time.posixToMillis (DateUtils.parseUTCDate "2024-01-02T11:00:00Z"))
                    in
                    Expect.equal (EventUtils.isEventOngoing now event) False
            ]
        ]