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
                makeFactorsTests
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
        , describe "collect"
            [ describe "parametric test" <|
                makeCollectTests
                    [ ( [], [] )
                    , ( [ 2 ], [ ( 2, 1 ) ] )
                    , ( [ 2, 2 ], [ ( 2, 2 ) ] )
                    , ( [ 2, 2, 2 ], [ ( 2, 3 ) ] )
                    , ( [ 2, 3 ], [ ( 2, 1 ), ( 3, 1 ) ] )
                    , ( [ 2, 2, 3 ], [ ( 2, 2 ), ( 3, 1 ) ] )
                    , ( [ 2, 2, 2, 3 ], [ ( 2, 3 ), ( 3, 1 ) ] )
                    , ( [ 2, 3, 3 ], [ ( 2, 1 ), ( 3, 2 ) ] )
                    , ( [ 2, 2, 3, 3 ], [ ( 2, 2 ), ( 3, 2 ) ] )
                    , ( [ 2, 2, 2, 3, 3 ], [ ( 2, 3 ), ( 3, 2 ) ] )
                    ]
            ]
        ]


makeFactorsTests : List ( Int, List Int ) -> List Test
makeFactorsTests =
    List.map makeFactorsTest


makeFactorsTest : ( Int, List Int ) -> Test
makeFactorsTest ( n, expected ) =
    test ("factors of " ++ String.fromInt n) <|
        \_ -> Expect.equal expected <| factors n


makeCollectTests : List ( List Int, List ( Int, Int ) ) -> List Test
makeCollectTests =
    List.map makeCollectTest


makeCollectTest : ( List Int, List ( Int, Int ) ) -> Test
makeCollectTest ( input, expected ) =
    let
        content =
            input
                |> List.map String.fromInt
                |> String.join ","

        testName =
            "[" ++ content ++ "]"
    in
    test ("collect of " ++ testName) <|
        \_ -> Expect.equal expected <| collect input
