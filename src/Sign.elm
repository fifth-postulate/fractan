module Sign exposing (sign)


sign : Int -> Int
sign n =
    case compare n 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1
