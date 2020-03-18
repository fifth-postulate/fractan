module SignTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int)
import Sign exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Sign module"
        [ describe "sign"
            [ fuzz2 int int "product of signs is sign of products" <|
                \a b ->
                    Expect.equal (sign a * sign b) (sign (a * b))
            ]
        ]
