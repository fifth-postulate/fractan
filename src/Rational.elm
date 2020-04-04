module Rational exposing (Error(..), Fraction, add, decode, divide, fraction, fromInt, integer, multiply, one, parse, subtract, toInt, toString, view, zero)

import Browser
import Css exposing (..)
import Gcd exposing (gcd)
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attribute
import Json.Decode as Decode exposing (Decoder, Value)
import Prime
import Show exposing (Show(..))
import Sign exposing (sign)


main =
    Browser.element
        { init = init
        , view = toUnstyled << internalView
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    Value


type alias Model =
    { f : Fraction
    , show : Show
    }


toModel : Show -> Int -> Int -> Model
toModel show numerator denominator =
    { show = show, f = safe_fraction numerator denominator }


type Fraction
    = Fraction { numerator : Int, denominator : Int }


init : Flags -> ( Model, Cmd msg )
init flags =
    let
        d =
            { show = Integral, f = one }

        m =
            flags
                |> Decode.decodeValue decodeFlags
                |> Result.withDefault d
    in
    ( m, Cmd.none )


one : Fraction
one =
    fromInt 1


zero : Fraction
zero =
    fromInt 0


fromInt : Int -> Fraction
fromInt n =
    Fraction { numerator = n, denominator = 1 }


toInt : Fraction -> Result Error Int
toInt (Fraction { numerator, denominator }) =
    if denominator == 1 then
        Ok numerator

    else
        Err NotAnInteger


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
    | NotAnInteger
    | ToFewParts
    | ToManyParts
    | PartsNotAnInt
    | NumeratorNotAnInt
    | DenominatorNotAnInt
    | NotAnFraction


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )


internalView : Model -> Html msg
internalView model =
    let
        v =
            case model.show of
                Factors ->
                    Prime.view

                Integral ->
                    \n -> Html.span [] [ Html.text <| String.fromInt n ]
    in
    view v model.f


view : (Int -> Html msg) -> Fraction -> Html msg
view v f =
    if integer f then
        viewInteger v f

    else
        viewFraction v f


viewFraction : (Int -> Html msg) -> Fraction -> Html msg
viewFraction v (Fraction { numerator, denominator }) =
    Html.div [ Attribute.css [ fractionStyle ] ]
        [ v numerator
        , Html.hr [ Attribute.css [ width (pct 100) ] ] []
        , v denominator
        ]


viewInteger : (Int -> Html msg) -> Fraction -> Html msg
viewInteger v (Fraction { numerator }) =
    Html.div [ Attribute.css [ fractionStyle ] ]
        [ v numerator
        ]


fractionStyle : Style
fractionStyle =
    Css.batch [ display inlineFlex, flexDirection column, flexWrap noWrap, justifyContent center, alignItems center ]


decodeFlags : Decoder Model
decodeFlags =
    Decode.map3 toModel
        (Decode.field "show" Show.decode)
        (Decode.field "numerator" Decode.int)
        (Decode.field "denominator" Decode.int)


decode : Decoder Fraction
decode =
    Decode.map (parse >> Result.withDefault zero)
        Decode.string


parse : String -> Result Error Fraction
parse input =
    input
        |> String.split "/"
        |> List.map String.toInt
        |> toFraction


toFraction : List (Maybe Int) -> Result Error Fraction
toFraction input =
    case input of
        [ Just n, Just d ] ->
            fraction n d

        [ _ ] ->
            Err ToFewParts

        [ Nothing, Nothing ] ->
            Err PartsNotAnInt

        [ Nothing, _ ] ->
            Err NumeratorNotAnInt

        [ _, Nothing ] ->
            Err DenominatorNotAnInt

        _ ->
            Err ToManyParts


toString : Fraction -> String
toString (Fraction { numerator, denominator }) =
    [ numerator, denominator ]
        |> List.map String.fromInt
        |> String.join "/"


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
