module Fractan exposing (..)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Json.Decode as Decode exposing (Decoder, Value)
import Prime
import Rational exposing (Fraction, fraction)
import Show exposing (Show(..))


main =
    Browser.element
        { init = init
        , view = toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Message )
init flags =
    let
        fs =
            [ fraction 1 2 ]
                |> List.map (Result.withDefault Rational.zero)

        p =
            program 2 fs

        default =
            exploration p
                |> primeFactors

        e =
            flags
                |> Decode.decodeValue decode
                |> Result.withDefault default
    in
    ( e, Cmd.none )


type alias Flags =
    Value


type alias Model =
    Exploration


type Exploration
    = Exploration
        { currentProgram : Program
        , originalState : Int
        , index : Maybe Int
        , seen : List Int
        , show : Show
        }


exploration : Program -> Exploration
exploration p =
    Exploration
        { currentProgram = p
        , originalState = number p
        , index = Nothing
        , seen = [ number p ]
        , show = Integral
        }


reset : Exploration -> Exploration
reset (Exploration ({ currentProgram, originalState } as e)) =
    Exploration { e | currentProgram = currentProgram |> withNumber originalState, index = Nothing, seen = [ originalState ] }


primeFactors : Exploration -> Exploration
primeFactors (Exploration e) =
    Exploration { e | show = Factors }


integral : Exploration -> Exploration
integral (Exploration e) =
    Exploration { e | show = Integral }


microStep : Exploration -> Exploration
microStep ((Exploration ({ currentProgram, index, seen } as data)) as e) =
    if finished currentProgram then
        e

    else
        case index of
            Nothing ->
                Exploration { data | index = Just 0 }

            Just i ->
                case nthFraction i currentProgram of
                    Nothing ->
                        macroStep e

                    Just f ->
                        if Rational.integer <| Rational.multiply f <| Rational.fromInt <| number currentProgram then
                            macroStep e

                        else
                            Exploration { data | index = Just <| i + 1 }


macroStep : Exploration -> Exploration
macroStep ((Exploration ({ currentProgram, seen } as data)) as e) =
    if finished currentProgram then
        e

    else
        let
            nextProgram =
                step currentProgram

            nextSeen =
                if finished nextProgram then
                    seen

                else
                    number nextProgram :: seen
        in
        Exploration { data | currentProgram = nextProgram, index = Nothing, seen = nextSeen }


state : Exploration -> Int
state (Exploration { currentProgram }) =
    number currentProgram


withState : Int -> Exploration -> Exploration
withState n (Exploration e) =
    Exploration { e | currentProgram = e.currentProgram |> withNumber n, originalState = n, index = Nothing, seen = [ n ] }


instructions : Exploration -> List Fraction
instructions (Exploration { currentProgram }) =
    fractions currentProgram


withInstructions : List Fraction -> Exploration -> Exploration
withInstructions fs (Exploration e) =
    Exploration { e | currentProgram = e.currentProgram |> withFractions fs, index = Nothing, seen = [ e.originalState ] }


type Program
    = Program { n : Int, fs : List Fraction, isFinished : Bool }


program : Int -> List Fraction -> Program
program n fs =
    Program { n = n, fs = fs, isFinished = False }


step : Program -> Program
step ((Program ({ n, fs } as data)) as p) =
    if finished p then
        p

    else
        let
            multiplyWithNumber =
                Rational.multiply <| Rational.fromInt n

            unwrap =
                Result.withDefault 0

            result =
                fs
                    |> List.map multiplyWithNumber
                    |> List.filter Rational.integer
                    |> List.map Rational.toInt
                    |> List.map unwrap
                    |> List.head
        in
        case result of
            Just nextNumber ->
                Program { data | n = nextNumber }

            Nothing ->
                Program { data | isFinished = True }


finished : Program -> Bool
finished (Program { isFinished }) =
    isFinished


number : Program -> Int
number (Program { n }) =
    n


withNumber : Int -> Program -> Program
withNumber n (Program p) =
    Program { p | n = n, isFinished = False }


fractions : Program -> List Fraction
fractions (Program { fs }) =
    fs


withFractions : List Fraction -> Program -> Program
withFractions fs (Program p) =
    Program { p | fs = fs }


type Message
    = Reset
    | MicroStep
    | MacroStep


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Reset ->
            ( reset model, Cmd.none )

        MicroStep ->
            ( microStep model, Cmd.none )

        MacroStep ->
            ( macroStep model, Cmd.none )


view : Exploration -> Html Message
view (Exploration { currentProgram, index, seen, show }) =
    let
        v =
            intView show
    in
    Html.div []
        [ viewControls
        , viewProgram v currentProgram
        , viewIntermediate v index currentProgram
        , viewSeen v seen
        ]


intView : Show -> Int -> Html msg
intView show n =
    case show of
        Factors ->
            Prime.view n

        Integral ->
            Html.span [] [ Html.text <| String.fromInt n ]


viewControls : Html Message
viewControls =
    Html.div []
        [ Html.button [ Event.onClick Reset ] [ Html.text "↻" ]
        , Html.button [ Event.onClick MicroStep ] [ Html.text "→" ]
        , Html.button [ Event.onClick MacroStep ] [ Html.text "↲" ]
        ]


viewIntermediate : (Int -> Html msg) -> Maybe Int -> Program -> Html msg
viewIntermediate v i p =
    case (Maybe.andThen <| swap nthFraction p) i of
        Nothing ->
            Html.div [] []

        Just f ->
            Html.div [ Attribute.css [ displayFlex, flexDirection row, flexWrap noWrap, justifyContent flexStart, alignItems center ] ]
                [ v <| number p
                , Html.span [] [ Html.text <| "⨉" ]
                , Rational.view v f
                , Html.span [ Attribute.css [ marginLeft <| em 0.5, marginRight <| em 0.5 ] ] [ Html.text <| "=" ]
                , Rational.view v <| Rational.multiply f <| Rational.fromInt <| number p
                ]


swap : (a -> b -> c) -> (b -> a -> c)
swap f =
    \b a -> f a b


nthFraction : Int -> Program -> Maybe Fraction
nthFraction i (Program { fs }) =
    fs
        |> List.drop i
        |> List.head


viewProgram : (Int -> Html msg) -> Program -> Html msg
viewProgram v (Program { n, fs }) =
    let
        comma =
            Html.span [] [ Html.text "," ]

        viewFractions =
            fs
                |> List.map (Rational.view v)
                |> List.intersperse comma
    in
    Html.div [ Attribute.css [ display inlineFlex, flexDirection row, flexWrap noWrap, justifyContent flexStart, alignItems center ] ]
        [ v n
        , Html.div [ Attribute.css [ marginLeft (em 1), display inlineFlex, flexDirection row, flexWrap noWrap, justifyContent flexStart, alignItems center ] ] viewFractions
        ]


viewSeen : (Int -> Html msg) -> List Int -> Html msg
viewSeen viewNumber ns =
    let
        comma =
            Html.span [] [ Html.text "," ]

        numbers =
            ns
                |> List.map viewNumber
                |> List.intersperse comma
    in
    Html.div [] numbers


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none


decode : Decoder Exploration
decode =
    Decode.at [ "description", "show" ] Show.decode
        |> Decode.andThen decodeDescription


decodeDescription : Show -> Decoder Exploration
decodeDescription show =
    let
        mapper e =
            case show of
                Factors ->
                    primeFactors e

                Integral ->
                    integral e
    in
    Decode.map exploration
        (Decode.field "description" decodeProgram)
        |> Decode.map mapper


decodeProgram : Decoder Program
decodeProgram =
    Decode.map2 program
        (Decode.field "number" Decode.int)
        (Decode.field "fractions" <| Decode.list Rational.decode)
