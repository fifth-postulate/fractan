module Fractan exposing (..)

import Html
import Rational exposing (Fraction)


main =
    let
        f =
            Rational.fraction 2 3
                |> Result.withDefault Rational.one

        g =
            Rational.fraction 3 5
                |> Result.withDefault Rational.one

        h =
            Rational.divide f g
                |> Result.withDefault Rational.zero
    in
    Html.div []
        [ Html.div []
            [ Rational.view f
            , Html.span [] [ Html.text "/" ]
            , Rational.view g
            , Html.span [] [ Html.text "=" ]
            , Rational.view h
            ]
        , Html.div []
            [ Rational.view f
            , Html.span [] [ Html.text "*" ]
            , Rational.view g
            , Html.span [] [ Html.text "=" ]
            , Rational.view <| Rational.multiply f g
            ]
        , Html.div []
            [ Rational.view f
            , Html.span [] [ Html.text "+" ]
            , Rational.view g
            , Html.span [] [ Html.text "=" ]
            , Rational.view <| Rational.add f g
            ]
        , Html.div []
            [ Rational.view f
            , Html.span [] [ Html.text "-" ]
            , Rational.view g
            , Html.span [] [ Html.text "=" ]
            , Rational.view <| Rational.subtract f g
            ]
        ]
