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

        model =
            { exploration = Fractan.exploration p }
    in
    ( model, Cmd.none )


type alias Model =
    { exploration : Exploration }


type Message
    = Machine Fractan.Message
    | Load


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Machine fractanMessage ->
            let
                ( exploration, _ ) =
                    Fractan.update fractanMessage model.exploration
            in
            ( { model | exploration = exploration }, Cmd.none )

        Load ->
            ( model, Cmd.none )


view : Model -> Html Message
view model =
    let
        e =
            model.exploration
                |> Fractan.view
                |> Html.map Machine
    in
    Html.div []
        [ e
        , viewControls model
        ]


viewControls : Model -> Html Message
viewControls model =
    let
        defaultState = 
            model.exploration
            |> Fractan.state
            |> String.fromInt

        defaultInstructions =
            model.exploration
            |> Fractan.instructions
            |> List.map Rational.toString
            |> String.join ", "
    in
    Html.div []
        [ Html.input [Attribute.placeholder defaultState ][] 
        , Html.input [Attribute.placeholder defaultInstructions][]
        ]


subscriptions : Model -> Sub Message
subscriptions _ =
    Sub.none
