module Prime exposing (factors)

import Browser
import Css exposing (..)
import Html.Styled as Html exposing (Html, toUnstyled)


main =
    Browser.element
        { init = init
        , view = toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    Int


type alias Model =
    Int

init: Flags -> (Model, Cmd Message)
init flags =
    (flags, Cmd.none)


type Message
    = DoNothing


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Html Message
view model =
    let
        times =
            Html.span [] [ Html.text "⨉"]
        content =
            model
                |> factors
                |> List.map String.fromInt
                |> List.map Html.text
                |> List.map (\t -> Html.span [] [ t ])
                |> List.intersperse times
    in
    Html.div [] content


factors : Int -> List Int
factors n =
    if n == 1 then
        [ 1 ]

    else
        factorsFrom [] 2 n


factorsFrom : List Int -> Int -> Int -> List Int
factorsFrom acc d n =
    if d > n then
        List.reverse acc

    else if modBy d n == 0 then
        factorsFrom (d :: acc) d (n // d)

    else
        factorsFrom acc (d + 1) n


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
