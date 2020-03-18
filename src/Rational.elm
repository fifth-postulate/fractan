module Rational exposing (Error, Fraction, add, divide, fraction, fromInt, integer, multiply, one, subtract, view, zero)

import Css exposing (..)
import Gcd exposing (gcd)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Sign exposing (sign)


type Fraction
    = Fraction { numerator : Int, denominator : Int }


one : Fraction
one =
    fromInt 1


zero : Fraction
zero =
    fromInt 0


fromInt : Int -> Fraction
fromInt n =
    Fraction { numerator = n, denominator = 1 }


fraction : Int -> Int -> Result Error Fraction
fraction numerator denominator =
    if denominator /= 0 then
        Ok <| safe_fraction numerator denominator

    else
        Err DivideByZero


safe_fraction : Int -> Int -> Fraction
safe_fraction p q =
    let
        a =
            abs p

        b =
            abs q

        s =
            sign <| p * q

        d =
            gcd a b

        numerator =
            s * (a // d)

        denominator =
            b // d
    in
    Fraction { numerator = numerator, denominator = denominator }


integer : Fraction -> Bool
integer (Fraction { denominator }) =
    denominator == 1


add : Fraction -> Fraction -> Fraction
add (Fraction f) (Fraction g) =
    let
        numerator =
            f.numerator * g.denominator + f.denominator * g.numerator

        denominator =
            f.denominator * g.denominator
    in
    safe_fraction numerator denominator


subtract : Fraction -> Fraction -> Fraction
subtract (Fraction f) (Fraction g) =
    let
        numerator =
            f.numerator * g.denominator - f.denominator * g.numerator

        denominator =
            f.denominator * g.denominator
    in
    safe_fraction numerator denominator


multiply : Fraction -> Fraction -> Fraction
multiply (Fraction f) (Fraction g) =
    let
        numerator =
            f.numerator * g.numerator

        denominator =
            f.denominator * g.denominator
    in
    safe_fraction numerator denominator


divide : Fraction -> Fraction -> Result Error Fraction
divide (Fraction f) (Fraction g) =
    let
        numerator =
            f.numerator * g.denominator

        denominator =
            f.denominator * g.numerator
    in
    fraction numerator denominator


type Error
    = DivideByZero


view : Fraction -> Html msg
view (Fraction { numerator, denominator }) =
    Html.div [ Attribute.css [ display inlineFlex, flexDirection column, flexWrap noWrap, justifyContent center, alignItems center ] ]
        [ Html.span [] [ Html.text <| String.fromInt numerator ]
        , Html.hr [ Attribute.css [ width (pct 100) ] ] []
        , Html.span [] [ Html.text <| String.fromInt denominator ]
        ]
