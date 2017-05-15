port module Main exposing (..)

import Basics.Interval
    exposing
        ( IntervalName(..)
        , interval
        , semitones
        )
import Html exposing (Html, text, div, input)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


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
    Sub.none


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type Msg
    = NoOp
    | PlayNote


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PlayNote ->
            ( model, playChord )


cMajorScaleChord : List (List Int)
cMajorScaleChord =
    [ PerfectUnison
    , MajorSecond
    , MajorThird
      -- , PerfectFourth
    , PerfectFifth
      -- , MajorSixth
    , MajorSeventh
      -- , PerfectOctave
    ]
        |> List.map (\i -> [ 2, semitones (interval i) + 60, 10 ])


playChord : Cmd Msg
playChord =
    cMajorScaleChord
        |> List.map sendNote
        |> Cmd.batch


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div
            [ onClick <| PlayNote
            ]
            [ text "C Major"
            ]
        ]
