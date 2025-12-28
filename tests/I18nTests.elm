module I18nTests exposing (..)

{-| Tests for I18n
Reference: agents/adr/0005-internationalization.md
ADRs: ADR-0012, ADR-0005
-}

import I18n
import Test exposing (..)
import Expect


suite : Test
suite =
    describe "I18n"
        [ describe "get"
            [ test "should return Finnish translation for known key" <|
                \_ ->
                    Expect.equal (I18n.get "calendar") "kalenteri"
            , test "should return Finnish translation for another known key" <|
                \_ ->
                    Expect.equal (I18n.get "today") "tänään"
            , test "should return key itself for unknown key" <|
                \_ ->
                    Expect.equal (I18n.get "unknown_key") "unknown_key"
            , test "should handle all main UI keys" <|
                \_ ->
                    let
                        keys = ["calendar", "list", "today", "prev", "next_button", "back"]
                        translations = List.map I18n.get keys
                    in
                    Expect.all
                        (List.map (\t -> \() -> Expect.notEqual t "") translations)
                        ()
            ]
        ]