module RationalTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Rational exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Rational module"
        [ describe "fractions"
            [ test "Fractions are normalized" <|
                \_ ->
                    let
                        f =
                            fraction 2 3

                        g =
                            fraction 4 6
                    in
                    Expect.equal f g
            , test "Fractions can be added" <|
                \_ ->
                    let
                        f =
                            fraction 1 2
                                |> Result.withDefault Rational.zero

                        g =
                            fraction 2 3
                                |> Result.withDefault Rational.zero

                        sum =
                            Rational.add f g

                        expected =
                            fraction 7 6
                                |> Result.withDefault Rational.one
                    in
                    Expect.equal sum expected
            , test "Fractions can be subtracted" <|
                \_ ->
                    let
                        f =
                            fraction 1 2
                                |> Result.withDefault Rational.zero

                        g =
                            fraction 2 3
                                |> Result.withDefault Rational.zero

                        difference =
                            Rational.subtract f g

                        expected =
                            fraction -1 6
                                |> Result.withDefault Rational.one
                    in
                    Expect.equal difference expected
            , test "Fractions can be multiplied" <|
                \_ ->
                    let
                        f =
                            fraction 1 2
                                |> Result.withDefault Rational.zero

                        g =
                            fraction 2 3
                                |> Result.withDefault Rational.zero

                        product =
                            Rational.multiply f g

                        expected =
                            fraction 1 3
                                |> Result.withDefault Rational.one
                    in
                    Expect.equal product expected
            , test "Fractions can be divided" <|
                \_ ->
                    let
                        f =
                            fraction 1 2
                                |> Result.withDefault Rational.zero

                        g =
                            fraction 2 3
                                |> Result.withDefault Rational.zero

                        quotient =
                            Rational.divide f g
                                |> Result.withDefault Rational.zero

                        expected =
                            fraction 3 4
                                |> Result.withDefault Rational.one
                    in
                    Expect.equal quotient expected
            , test "Fractions can be tested for integer" <|
                \_ ->
                    let
                        f =
                            fraction 1 2
                                |> Result.withDefault Rational.zero

                        g =
                            fraction 2 4
                                |> Result.withDefault Rational.zero

                        isInteger =
                            Rational.divide f g
                                |> Result.map integer
                                |> Result.withDefault False
                    in
                    Expect.true "fraction to be integer" isInteger
            , test "Fractions can be parsed" <|
                \_ ->
                    let
                        result =
                            parse "1/3"

                        expected =
                            fraction 1 3
                    in
                    Expect.equal result expected
            , test "Fractions can be decoded" <|
                \_ ->
                    let
                        decoder =
                            Decode.field "fraction" decode

                        result =
                            Decode.decodeString decoder """{"fraction":"1/2"}"""
                                |> Result.mapError (\_ -> Rational.NotAnFraction)

                        expected =
                            fraction 1 2
                    in
                    Expect.equal result expected
            ]
        ]
