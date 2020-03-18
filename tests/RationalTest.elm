module RationalTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Rational exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Rational module"
        [ describe "sign"
            [ fuzz2 int int "product of signs is sign of products" <|
                \a b ->
                    Expect.equal (sign a * sign b) (sign (a * b))
            ]
        , describe "gcd"
            [ fuzz int "gcd of n and zero is n" <|
                \n ->
                    Expect.equal (gcd n 0) n
            , fuzz int "gcd of n and n is n" <|
                \n ->
                    Expect.equal (gcd n n) n
            , test "gcd of 51 and 37 is 1" <|
                \_ ->
                    Expect.equal (gcd 51 37) 1
            ]
        , describe "fractions"
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
            ]
        ]
