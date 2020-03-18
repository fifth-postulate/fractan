module Rational exposing (Error, Fraction, add, divide, fraction, fromInt, gcd, multiply, one, sign, subtract, view, zero)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute


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


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b <| modBy b a


sign : Int -> Int
sign n =
    case compare n 0 of
        LT ->
            -1

        EQ ->
            0

        GT ->
            1


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
        , Html.span [] [ Html.text <| String.fromInt denominator ]
        ]
