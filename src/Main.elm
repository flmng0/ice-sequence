module Main exposing (main)

import Browser
import Elements exposing (Element, generate)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random



-- INTERFACE CODE


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type State
    = Waiting
    | Sequence (List Element)


type alias Model =
    { state : State
    , count : Int
    }


type Msg
    = NewSequence (List Element)
    | Regenerate
    | SetCount Int


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialCount =
            30
    in
    ( Model Waiting initialCount, Random.generate NewSequence (generate initialCount) )


view model =
    case model.state of
        Waiting ->
            main_ [] [ text "Waiting for sequence to generate" ]

        Sequence sequence ->
            main_ []
                [ showControl model
                , showSequence sequence
                ]


update msg model =
    case msg of
        NewSequence sequence ->
            ( { model | state = Sequence sequence }, Cmd.none )

        Regenerate ->
            ( model, Random.generate NewSequence (generate model.count) )

        SetCount count ->
            ( { model | count = clamp 2 99 count }, Cmd.none )


subscriptions _ =
    Sub.none



-- FINE GRAINED COMPONENTS


showControl : Model -> Html Msg
showControl { count } =
    div [ class "controls" ]
        [ button [ class "generate", onClick Regenerate ] [ text "Generate!" ]
        , div [ class "count" ]
            [ button [ onClick (SetCount (count - 10)) ] [ text "-10" ]
            , button [ onClick (SetCount (count - 1)) ] [ text "-" ]
            , span [] [ text "Count: ", pre [] [ text (String.fromInt count |> String.padLeft 2 ' ') ] ]
            , button [ onClick (SetCount (count + 1)) ] [ text "+" ]
            , button [ onClick (SetCount (count + 10)) ] [ text "+10" ]
            ]
        ]


showSequence : List Element -> Html Msg
showSequence sequence =
    ol [ class "sequence" ] (List.map showElement sequence)


showElement : Element -> Html Msg
showElement m =
    li [ class "sequence-item" ]
        [ text (Elements.displayName m True)
        ]
