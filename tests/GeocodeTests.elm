module GeocodeTests exposing (..)

{-| Tests for Geocode
Reference: agents/stories/create-event.md
ADRs: ADR-0012
-}

import Expect
import Geocode
import Json.Decode as Decode
import Test exposing (..)


suite : Test
suite =
    describe "Geocode"
        [ describe "resultDecoder"
            [ test "should decode valid coordinates JSON" <|
                \_ ->
                    let
                        json =
                            """{"lat": "60.1699", "lon": "24.9384"}"""

                        result =
                            Decode.decodeString Geocode.resultDecoder json
                    in
                    Expect.equal result (Ok ( 60.1699, 24.9384 ))
            , test "should fail on invalid latitude" <|
                \_ ->
                    let
                        json =
                            """{"lat": "invalid", "lon": "24.9384"}"""

                        result =
                            Decode.decodeString Geocode.resultDecoder json
                    in
                    case result of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Expected decoding to fail"
            , test "should fail on invalid longitude" <|
                \_ ->
                    let
                        json =
                            """{"lat": "60.1699", "lon": "invalid"}"""

                        result =
                            Decode.decodeString Geocode.resultDecoder json
                    in
                    case result of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Expected decoding to fail"
            ]
        , describe "maybeToDecoder"
            [ test "should succeed with Just value" <|
                \_ ->
                    let
                        decoder =
                            Geocode.maybeToDecoder (Just 42)

                        result =
                            Decode.decodeString decoder "\"test\""
                    in
                    Expect.equal result (Ok 42)
            , test "should fail with Nothing" <|
                \_ ->
                    let
                        decoder =
                            Geocode.maybeToDecoder Nothing

                        result =
                            Decode.decodeString decoder "\"test\""
                    in
                    case result of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Expected decoding to fail"
            ]
        ]
