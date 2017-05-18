port module Main exposing (..)

import Composition.Part exposing (Part, calculate)
import Basics.Interval exposing (IntervalName(..), interval, semitones)
import Html exposing (Html, text, div, input)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Time exposing (Time, every, millisecond)


port sendNote : List Int -> Cmd msg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing == True then
        every (millisecond * model.tempo) PlayNextNote
    else
        Sub.none


type alias Model =
    { sequence : List (List Int)
    , playing : Bool
    , nextNotes : List (List Int)
    , tempo : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { sequence =
            [ [ 60, 70 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 65, 71 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 60, 72 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 65, 73 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 60, 74 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 68, 75 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 61, 76 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 62, 77 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 64, 78 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 69, 79 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 68, 80 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            , [ 62, 81 ]
            , [ 0 ]
            , [ 0 ]
            , [ 0 ]
            ]
      , playing = False
      , nextNotes = []
      , tempo = 20
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | PlayNextNote Time
    | TogglePlay
    | StartPlaying


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PlayNextNote _ ->
            let
                nextNote =
                    (List.head model.nextNotes)
            in
                ( { model | nextNotes = (List.drop 1 model.nextNotes) }
                , sendNoteIfNotEmpty model (List.head model.nextNotes)
                )

        TogglePlay ->
            let
                newModel =
                    { model | playing = not model.playing }
            in
                if model.playing == True then
                    ( newModel, Cmd.none )
                else
                    update StartPlaying newModel

        StartPlaying ->
            (update (PlayNextNote millisecond)
                { model
                    | nextNotes = model.sequence
                }
            )


sendNoteIfNotEmpty : Model -> Maybe (List Int) -> Cmd Msg
sendNoteIfNotEmpty model notes =
    case notes of
        Just ns ->
            List.map
                (\n ->
                    if n > 10 then
                        sendNote [ 2, n, round (model.tempo) ]
                    else
                        Cmd.none
                )
                ns
                |> Cmd.batch

        Nothing ->
            Cmd.none


view : Model -> Html Msg
view model =
    let
        buttonLabel =
            if model.playing == True then
                "Pause"
            else
                "play"
    in
        div [ class "app" ]
            [ div
                [ onClick <| TogglePlay
                ]
                [ text buttonLabel
                ]
            ]
