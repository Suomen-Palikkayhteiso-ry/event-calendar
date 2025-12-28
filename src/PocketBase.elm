module PocketBase exposing (..)

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Types exposing (Auth, Event, User)



-- Configuration


baseUrl : String
baseUrl =
    "https://data.suomenpalikkayhteiso.fi/api"



-- Authentication


type alias LoginCredentials =
    { email : String
    , password : String
    }


login : LoginCredentials -> (Result Http.Error Auth -> msg) -> Cmd msg
login credentials toMsg =
    Http.post
        { url = baseUrl ++ "/collections/users/auth-with-password"
        , body = Http.jsonBody (loginEncoder credentials)
        , expect = Http.expectJson toMsg authResponseDecoder
        }


initiateOAuth2Login : String -> Cmd msg
initiateOAuth2Login provider =
    -- For OAuth2, we need to redirect the user to the OAuth2 provider
    -- This is typically done by setting window.location.href
    -- We'll use a port to handle this on the JavaScript side
    Ports.initiateOAuth2Login provider


loginEncoder : LoginCredentials -> Encode.Value
loginEncoder credentials =
    Encode.object
        [ ( "identity", Encode.string credentials.email )
        , ( "password", Encode.string credentials.password )
        ]


authWithOAuth2Code : String -> Maybe String -> (Result Http.Error Auth -> msg) -> Cmd msg
authWithOAuth2Code code state toMsg =
    Http.post
        { url = baseUrl ++ "/oauth2-redirect"
        , body = Http.jsonBody (oauth2Encoder code state)
        , expect = Http.expectJson toMsg authResponseDecoder
        }


oauth2Encoder : String -> Maybe String -> Encode.Value
oauth2Encoder code state =
    Encode.object
        [ ( "code", Encode.string code )
        , ( "state", Encode.string (Maybe.withDefault "" state) )
        ]


authResponseDecoder : Decode.Decoder Auth
authResponseDecoder =
    Decode.map2 Auth
        (Decode.field "record" (Decode.nullable userDecoder))
        (Decode.field "token" (Decode.nullable Decode.string))


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map5 User
        (Decode.field "id" Decode.string)
        (Decode.field "email" Decode.string)
        (Decode.field "username" (Decode.nullable Decode.string))
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "avatar" (Decode.nullable Decode.string))


logout : String -> (Result Http.Error () -> msg) -> Cmd msg
logout token toMsg =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = baseUrl ++ "/collections/users/auth-refresh"
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- Event CRUD


getEvents : Maybe String -> (Result Http.Error (List Event) -> msg) -> Cmd msg
getEvents token toMsg =
    Http.request
        { method = "GET"
        , headers = authHeader token
        , url = baseUrl ++ "/collections/events/records"
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg eventsResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


eventsResponseDecoder : Decode.Decoder (List Event)
eventsResponseDecoder =
    Decode.field "items" (Decode.list Types.eventDecoder)


createEvent : Maybe String -> Event -> (Result Http.Error Event -> msg) -> Cmd msg
createEvent token event toMsg =
    Http.request
        { method = "POST"
        , headers = authHeader token
        , url = baseUrl ++ "/collections/events/records"
        , body = Http.jsonBody (Types.eventEncoder event)
        , expect = Http.expectJson toMsg Types.eventDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateEvent : Maybe String -> String -> Event -> (Result Http.Error Event -> msg) -> Cmd msg
updateEvent token id event toMsg =
    Http.request
        { method = "PATCH"
        , headers = authHeader token
        , url = baseUrl ++ "/collections/events/records/" ++ id
        , body = Http.jsonBody (Types.eventEncoder event)
        , expect = Http.expectJson toMsg Types.eventDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteEvent : Maybe String -> String -> (Result Http.Error () -> msg) -> Cmd msg
deleteEvent token id toMsg =
    Http.request
        { method = "DELETE"
        , headers = authHeader token
        , url = baseUrl ++ "/collections/events/records/" ++ id
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
