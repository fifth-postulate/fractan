module Fractan exposing (..)

import Css exposing (..)
import Html as BasicHtml
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attribute
import Rational exposing (Fraction, fraction)


main =
    let
        fractions =
            [ fraction 1 2, fraction 1 3, fraction 1 5 ]
                |> List.map (Result.withDefault Rational.zero)

        p =
            program 30 fractions

        e =
            exploration p
    in
    toUnstyled <| view e


type Exploration
    = Exploration
        { currentProgram : Program
        , index : Maybe Int
        , seen : List Int
        }


exploration : Program -> Exploration
exploration p =
    Exploration
        { currentProgram = p
        , index = Nothing
        , seen = []
        }


microStep : Exploration -> Exploration
microStep ((Exploration { currentProgram, index, seen }) as e) =
    if finished currentProgram then
        e

    else
        e


type Program
    = Program { number : Int, fractions : List Fraction, isFinished : Bool }


program : Int -> List Fraction -> Program
program n fs =
    Program { number = n, fractions = fs, isFinished = False }


step : Program -> Program
step ((Program ({ number, fractions, isFinished } as data)) as p) =
    if finished p then
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
                Program { data | number = nextNumber }

            Nothing ->
                Program { data | isFinished = True }


finished : Program -> Bool
finished (Program { isFinished }) =
    isFinished


view : Exploration -> Html msg
view (Exploration { currentProgram, index, seen }) =
    Html.div []
        [ viewProgram currentProgram
        ]


viewProgram : Program -> Html msg
viewProgram (Program { number, fractions }) =
    let
        comma =
            Html.span [] [ Html.text "," ]

        fs =
            fractions
                |> List.map Rational.view
                |> List.intersperse comma
    in
    Html.div [ Attribute.css [ display inlineFlex, flexDirection row, flexWrap noWrap, justifyContent flexStart, alignItems center ] ]
        [ Html.span [] [ Html.text <| String.fromInt number ]
        , Html.div [ Attribute.css [ display inlineFlex, flexDirection row, flexWrap noWrap, justifyContent flexStart, alignItems center ] ] fs
        ]
