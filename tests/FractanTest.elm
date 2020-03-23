module FractanTest exposing (..)

import Expect exposing (Expectation)
import Fractan exposing (..)
import Fuzz exposing (Fuzzer, int)
import Json.Decode as Decode
import Rational exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Fractan module"
        [ describe "Exploration"
            [ test "can be decoded" <|
                \_ ->
                    let
                        result =
                            Decode.decodeString Fractan.decode """{"description":{"number": 30,"fractions":["1/2","1/3","1/5"]}}"""

                        fractions =
                            [ fraction 1 2, fraction 1 3, fraction 1 5 ]
                                |> List.map (Result.withDefault Rational.zero)

                        p =
                            program 30 fractions

                        expected =
                            Ok <| exploration p
                    in
                    Expect.equal result expected
            ]
        , describe "Program"
            [ test "should correctly step" <|
                \_ ->
                    let
                        fractions =
                            [ fraction 1 2, fraction 1 3, fraction 1 5 ]
                                |> List.map (Result.withDefault Rational.zero)

                        result =
                            program 30 fractions
                                |> step

                        expected =
                            program 15 fractions
                    in
                    Expect.equal result expected
            , test "should correctly step multiple times" <|
                \_ ->
                    let
                        fractions =
                            [ fraction 1 2, fraction 1 3, fraction 1 5 ]
                                |> List.map (Result.withDefault Rational.zero)

                        result =
                            program 30 fractions
                                |> step
                                |> step

                        expected =
                            program 5 fractions
                    in
                    Expect.equal result expected
            ]
        ]
