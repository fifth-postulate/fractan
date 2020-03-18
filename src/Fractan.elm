module Fractan exposing (..)

import Css exposing (..)
import Html as BasicHtml
import Html.Styled as Html exposing (toUnstyled)
import Html.Styled.Attributes as Attribute
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
    toUnstyled <|
        Html.div []
            [ Html.div [ Attribute.css [ equation ] ]
                [ Rational.view f
                , Html.span [] [ Html.text "/" ]
                , Rational.view g
                , Html.span [] [ Html.text "=" ]
                , Rational.view h
                ]
            , Html.div [ Attribute.css [ equation ] ]
                [ Rational.view f
                , Html.span [] [ Html.text "*" ]
                , Rational.view g
                , Html.span [] [ Html.text "=" ]
                , Rational.view <| Rational.multiply f g
                ]
            , Html.div [ Attribute.css [ equation ] ]
                [ Rational.view f
                , Html.span [] [ Html.text "+" ]
                , Rational.view g
                , Html.span [] [ Html.text "=" ]
                , Rational.view <| Rational.add f g
                ]
            , Html.div [ Attribute.css [ equation ] ]
                [ Rational.view f
                , Html.span [] [ Html.text "-" ]
                , Rational.view g
                , Html.span [] [ Html.text "=" ]
                , Rational.view <| Rational.subtract f g
                ]
             , Html.div [ Attribute.css [ equation ] ]
                [ Rational.view f
                , Html.span [] [ Html.text "*" ]
                , Rational.view <| Rational.fromInt 3
                , Html.span [] [ Html.text "=" ]
                , Rational.view <| Rational.multiply f (Rational.fromInt 3)
                ]
            ]


equation : Style
equation =
    batch [ displayFlex, flexDirection row, flexWrap noWrap, justifyContent flexStart, alignItems center ]
