module Prime exposing (factors)


factors : Int -> List Int
factors n =
    if n == 1 then
        [ 1 ]

    else
        factorsFrom [] 2 n


factorsFrom : List Int -> Int -> Int -> List Int
factorsFrom acc d n =
    if d > n then
        List.reverse acc

    else if modBy d n == 0 then
        factorsFrom (d :: acc) d (n // d)

    else
        factorsFrom acc (d + 1) n
