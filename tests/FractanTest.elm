module FractanTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int)
import Fractan exposing (..)
import Rational exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Fractan module"
        [ describe "Program"
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
