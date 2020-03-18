module Fractan exposing (..)

import Css exposing (..)
import Html as BasicHtml
import Html.Styled as Html exposing (toUnstyled)
import Html.Styled.Attributes as Attribute
import Rational exposing (Fraction)

type Program =
    Program { number: Int, fractions: List Fraction, finished : Bool }

program : Int -> List Fraction -> Program
program n fs =
    Program { number = n, fractions = fs, finished = False }

step : Program -> Program
step ((Program ({ number, fractions, finished } as data)) as p) =
    if finished then
        p
    else
        let
            multiplyWithNumber =
                Rational.multiply <| Rational.fromInt number

            unwrap =
                Result.withDefault 0 

            result = 
                fractions
                |> List.map multiplyWithNumber
                |> List.filter Rational.integer
                |> List.map Rational.toInt
                |> List.map unwrap
                |> List.head
        in
            case result of
                Just nextNumber ->
                    Program { data | number = nextNumber}

                Nothing ->
                    Program { data | finished = True }

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
