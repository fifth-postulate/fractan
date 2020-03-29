module Show exposing (Show(..), decode)

import Json.Decode as Decode exposing (Decoder)


type Show
    = Integral
    | Fractional


decode : Decoder Show
decode =
    Decode.map toShow Decode.string


toShow : String -> Show
toShow input =
    case input of
        "fractional" ->
            Fractional

        _ ->
            Integral
