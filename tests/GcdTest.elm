module GcdTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int)
import Gcd exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Gcd module"
        [ describe "gcd"
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
        ]
