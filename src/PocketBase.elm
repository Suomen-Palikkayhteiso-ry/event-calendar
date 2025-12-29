module PocketBase exposing (..)

import Http
import Json.Decode as Decode
import Types exposing (Event)



-- Configuration


defaultBaseUrl : String
defaultBaseUrl =
    "https://data.suomenpalikkayhteiso.fi/api"


baseUrl : String -> String
baseUrl url =
    if String.isEmpty url then
        defaultBaseUrl

    else
        url



-- Event CRUD


getEvents : String -> Maybe String -> (Result Http.Error (List Event) -> msg) -> Cmd msg
getEvents pocketbaseUrl token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl pocketbaseUrl ++ "/collections/events/records"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg eventsResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getEvent : String -> Maybe String -> String -> (Result Http.Error Event -> msg) -> Cmd msg
getEvent pocketbaseUrl token id toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl pocketbaseUrl ++ "/collections/events/records/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg Types.eventDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


eventsResponseDecoder : Decode.Decoder (List Event)
eventsResponseDecoder =
    Decode.field "items" (Decode.list Types.eventDecoder)


createEvent : String -> Maybe String -> Event -> (Result Http.Error Event -> msg) -> Cmd msg
createEvent pocketbaseUrl token event toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader token
        , url = baseUrl pocketbaseUrl ++ "/collections/events/records"
        , body = Http.jsonBody (Types.eventEncoder event)
        , expect = Http.expectJson toMsg Types.eventDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateEvent : String -> Maybe String -> String -> Event -> (Result Http.Error Event -> msg) -> Cmd msg
updateEvent pocketbaseUrl token id event toMsg =
    Http.request
        { method = "PATCH"
        , headers = authHeader token
        , url = baseUrl pocketbaseUrl ++ "/collections/events/records/" ++ id
        , body = Http.jsonBody (Types.eventEncoder event)
        , expect = Http.expectJson toMsg Types.eventDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteEvent : String -> Maybe String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteEvent pocketbaseUrl token id toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeader token
        , url = baseUrl pocketbaseUrl ++ "/collections/events/records/" ++ id
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


authHeader : Maybe String -> List Http.Header
authHeader token =
    case token of
        Just t ->
            [ Http.header "Authorization" ("Bearer " ++ t) ]

        Nothing ->
            []
