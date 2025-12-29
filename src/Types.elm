module Types exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Json.Encode as Encode



-- Event


type alias Event =
    { id : String
    , title : String
    , description : Maybe String
    , startDate : String
    , endDate : Maybe String
    , allDay : Bool
    , url : Maybe String
    , location : Maybe String
    , state : EventState
    , image : Maybe String
    , imageDescription : Maybe String
    , point : Maybe Point
    , created : String
    , updated : String
    }


type EventState
    = Draft
    | Pending
    | Published
    | Deleted


type alias Point =
    { lat : Float
    , lon : Float
    }


type alias EventFormData =
    { title : String
    , start_date : String
    , end_date : String
    , all_day : Bool
    , location : String
    , description : String
    , url : String
    , image : Maybe String
    , image_description : String
    , state : EventState
    , point : Maybe Point
    }


type alias DisplayEvent =
    { event : Event
    , displayDate : String
    , displayTime : String
    }


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.succeed Event
        |> DecodeExtra.andMap (Decode.field "id" Decode.string)
        |> DecodeExtra.andMap (Decode.field "title" Decode.string)
        |> DecodeExtra.andMap (DecodeExtra.optionalField "description" (Decode.nullable Decode.string) |> Decode.map (Maybe.withDefault Nothing))
        |> DecodeExtra.andMap (Decode.field "start_date" Decode.string)
        |> DecodeExtra.andMap (DecodeExtra.optionalField "end_date" (Decode.nullable Decode.string) |> Decode.map (Maybe.withDefault Nothing))
        |> DecodeExtra.andMap (Decode.field "all_day" Decode.bool)
        |> DecodeExtra.andMap (DecodeExtra.optionalField "url" (Decode.nullable Decode.string) |> Decode.map (Maybe.withDefault Nothing))
        |> DecodeExtra.andMap (DecodeExtra.optionalField "location" (Decode.nullable Decode.string) |> Decode.map (Maybe.withDefault Nothing))
        |> DecodeExtra.andMap (Decode.field "state" eventStateDecoder)
        |> DecodeExtra.andMap (DecodeExtra.optionalField "image" (Decode.nullable Decode.string) |> Decode.map (Maybe.withDefault Nothing))
        |> DecodeExtra.andMap (DecodeExtra.optionalField "image_description" (Decode.nullable Decode.string) |> Decode.map (Maybe.withDefault Nothing))
        |> DecodeExtra.andMap (DecodeExtra.optionalField "point" (Decode.nullable pointDecoder) |> Decode.map (Maybe.withDefault Nothing))
        |> DecodeExtra.andMap (Decode.field "created" Decode.string)
        |> DecodeExtra.andMap (Decode.field "updated" Decode.string)


eventEncoder : Event -> Encode.Value
eventEncoder event =
    Encode.object
        [ ( "id", Encode.string event.id )
        , ( "title", Encode.string event.title )
        , ( "description", maybeStringEncoder event.description )
        , ( "start_date", Encode.string event.startDate )
        , ( "end_date", maybeStringEncoder event.endDate )
        , ( "all_day", Encode.bool event.allDay )
        , ( "url", maybeStringEncoder event.url )
        , ( "location", maybeStringEncoder event.location )
        , ( "state", eventStateEncoder event.state )
        , ( "image", maybeStringEncoder event.image )
        , ( "image_description", maybeStringEncoder event.imageDescription )
        , ( "point", maybePointEncoder event.point )
        , ( "created", Encode.string event.created )
        , ( "updated", Encode.string event.updated )
        ]


maybeStringEncoder : Maybe String -> Encode.Value
maybeStringEncoder maybe =
    case maybe of
        Just s ->
            Encode.string s

        Nothing ->
            Encode.null


maybePointEncoder : Maybe Point -> Encode.Value
maybePointEncoder maybe =
    case maybe of
        Just p ->
            pointEncoder p

        Nothing ->
            Encode.null


eventStateDecoder : Decode.Decoder EventState
eventStateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "draft" ->
                        Decode.succeed Draft

                    "pending" ->
                        Decode.succeed Pending

                    "published" ->
                        Decode.succeed Published

                    "deleted" ->
                        Decode.succeed Deleted

                    _ ->
                        Decode.fail ("Unknown event state: " ++ str)
            )


eventStateEncoder : EventState -> Encode.Value
eventStateEncoder state =
    Encode.string <|
        case state of
            Draft ->
                "draft"

            Pending ->
                "pending"

            Published ->
                "published"

            Deleted ->
                "deleted"


pointDecoder : Decode.Decoder Point
pointDecoder =
    Decode.map2 Point
        (Decode.field "lat" Decode.float)
        (Decode.field "lon" Decode.float)


pointEncoder : Point -> Encode.Value
pointEncoder point =
    Encode.object
        [ ( "lat", Encode.float point.lat )
        , ( "lon", Encode.float point.lon )
        ]



-- User / Auth types


type alias User =
    { id : String
    , email : String
    , username : Maybe String
    , name : Maybe String
    , avatar : Maybe String
    }


type alias Auth =
    { user : Maybe User
    , token : Maybe String
    }
