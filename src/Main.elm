module Main exposing (main)

import Browser
import Elements exposing (Element, generate)
import Html
import Random



-- INTERFACE CODE


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type Model
    = Waiting
    | Sequence (List Element)


type Msg
    = NewSequence (List Element)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Waiting, Random.generate NewSequence (generate 20) )


view model =
    case model of
        Waiting ->
            Html.div [] [ Html.text "Waiting for sequence to generate" ]

        Sequence sequence ->
            showSequence sequence


update msg model =
    case msg of
        NewSequence sequence ->
            ( Sequence sequence, Cmd.none )


subscriptions _ =
    Sub.none


showSequence : List Element -> Html.Html msg
showSequence sequence =
    Html.ul [] (List.map showMove sequence)


showMove : Element -> Html.Html msg
showMove m =
    Html.li []
        [ Html.text (Elements.displayName m True)
        ]
