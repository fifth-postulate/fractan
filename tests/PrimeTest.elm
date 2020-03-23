module PrimeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange)
import Prime exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Prime module"
        [ describe "factors"
            [ fuzz (intRange 2 10000) "factors of n and multiply to is n" <|
                 \n ->
                     Expect.equal n (List.product <| factors n)
              
            , describe "parametric test" <|
                    makeTests
                        [ ( 1, [ 1 ] )
                        , ( 2, [ 2 ] )
                        , ( 3, [ 3 ] )
                        , ( 4, [ 2, 2 ] )
                        , ( 5, [ 5 ] )
                        , ( 6, [ 2, 3 ] )
                        , ( 7, [ 7 ] )
                        , ( 8, [ 2, 2, 2 ] )
                        , ( 9, [ 3, 3 ] )
                        ]
            ]
        ]


makeTests : List ( Int, List Int ) -> List Test
makeTests =
    List.map makeTest


makeTest : ( Int, List Int ) -> Test
makeTest ( n, expected ) =
    test ("factors of " ++ String.fromInt n) <|
        \_ -> Expect.equal expected <| factors n
