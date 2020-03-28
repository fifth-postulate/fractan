module Prime exposing (collect, factors, view)

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


init : Flags -> ( Model, Cmd Message )
init flags =
    ( flags, Cmd.none )


type Message
    = DoNothing


update : Message -> Model -> ( Model, Cmd Message )
update _ model =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    let
        times =
            Html.span [] [ Html.text "·" ]

        content =
            model
                |> factors
                |> collect
                |> List.map viewPrimePower
                |> List.intersperse times
    in
    Html.div [] content


viewPrimePower : ( Int, Int ) -> Html msg
viewPrimePower ( base, exponent ) =
    if exponent == 1 then
        Html.span [] [ Html.text <| String.fromInt base ]

    else
        Html.span [] [ Html.text <| String.fromInt base, Html.sup [] [ Html.text <| String.fromInt exponent ] ]


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


collect : List Int -> List ( Int, Int )
collect =
    collectIn []


collectIn : List ( Int, Int ) -> List Int -> List ( Int, Int )
collectIn accumulator ns =
    case ( accumulator, ns ) of
        ( any, [] ) ->
            List.reverse any

        ( [], x :: xs ) ->
            collectIn [ ( x, 1 ) ] xs

        ( (( y, d ) as p) :: ps, x :: xs ) ->
            if x == y then
                collectIn (( x, d + 1 ) :: ps) xs

            else
                collectIn (( x, 1 ) :: p :: ps) xs


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none
