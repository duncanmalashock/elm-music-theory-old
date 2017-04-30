module Chord exposing (..)

import Note exposing (Note(..))
import Scale exposing (..)


type alias Chord =
    List Note


diatonicTriad : Scale -> Int -> Chord
diatonicTriad scale degree =
    [ scaleDegree scale degree
    , scaleDegree scale <| degree + 2
    , scaleDegree scale <| degree + 4
    ]


diatonicSeventhChord : Scale -> Int -> Chord
diatonicSeventhChord scale degree =
    [ scaleDegree scale degree
    , scaleDegree scale <| degree + 2
    , scaleDegree scale <| degree + 4
    , scaleDegree scale <| degree + 6
    ]
