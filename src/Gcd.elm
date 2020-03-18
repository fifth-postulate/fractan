module Gcd exposing (gcd)


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b <| modBy b a
