module Show exposing (Show(..), decode)

import Json.Decode as Decode exposing (Decoder)


type Show
    = Integral
    | Factors


decode : Decoder Show
decode =
    Decode.map toShow Decode.string


toShow : String -> Show
toShow input =
    case input of
        "factors" ->
            Factors

        _ ->
            Integral
