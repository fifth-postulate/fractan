module Playground exposing (..)

import Browser
import Css exposing (..)
import Fractan exposing (Exploration)
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


init : () -> ( Model, Cmd Message )
init _ =
    let
        fractions =
            [ fraction 1 2 ]
                |> List.map (Result.withDefault Rational.zero)

        p =
            Fractan.program 2 fractions

        pr =
            fractions
                |> List.map Rational.toString
                |> String.join ","

        model =
            { exploration = Fractan.exploration p, problem = Nothing, proto = pr }
    in
    ( model, Cmd.none )


type alias Model =
    { exploration : Exploration
    , problem : Maybe String
    , proto : String
    }


type Message
    = Machine Fractan.Message
    | ChangeState String
    | UpdateProto String
    | ChangeInstructions


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Machine fractanMessage ->
            let
                ( exploration, _ ) =
                    Fractan.update fractanMessage model.exploration
            in
            ( { model | exploration = exploration }, Cmd.none )

        ChangeState input ->
            let
                state =
                    String.toInt input

                nextModel =
                    case state of
                        Just n ->
                            { model | exploration = model.exploration |> Fractan.withState n, problem = Nothing }

                        Nothing ->
                            { model | problem = Just <| "\"" ++ input ++ "\" is not a number" }
            in
            ( nextModel, Cmd.none )

        UpdateProto input ->
            ( { model | proto = input }, Cmd.none )

        ChangeInstructions ->
            let
                state =
                    parseFractions model.proto

                nextModel =
                    case state of
                        Just fs ->
                            { model | exploration = model.exploration |> Fractan.withInstructions fs, problem = Nothing }

                        Nothing ->
                            { model | problem = Just <| "\"" ++ model.proto ++ "\" is not a list of fractions" }
            in
            ( nextModel, Cmd.none )


parseFractions : String -> Maybe (List Fraction)
parseFractions input =
    let
        attempt =
            input
                |> String.split ","
                |> List.map Rational.parse

        isError r =
            case r of
                Ok _ ->
                    False

                Err _ ->
                    True

        result =
            if List.any isError attempt then
                Nothing

            else
                attempt
                    |> List.map (Result.withDefault Rational.zero)
                    |> Just
    in
    result


view : Model -> Html Message
view model =
    let
        e =
            model.exploration
                |> Fractan.view
                |> Html.map Machine
    in
    Html.div []
        [ viewControls model
        , viewProblem model.problem
        , e
        ]


viewControls : Model -> Html Message
viewControls model =
    let
        defaultState =
            model.exploration
                |> Fractan.state
                |> String.fromInt

        defaultInstructions =
            model.proto
    in
    Html.div []
        [ Html.input [ Attribute.placeholder "State", Attribute.value defaultState, Event.onInput ChangeState ] []
        , Html.input [ Attribute.placeholder "Instructions", Attribute.value defaultInstructions, Event.onInput UpdateProto, Event.onBlur ChangeInstructions ] []
        ]


viewProblem : Maybe String -> Html Message
viewProblem problem =
    let
        content =
            problem
                |> Maybe.map (\s -> [ Html.text s ])
                |> Maybe.withDefault []
    in
    Html.div [] content


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
