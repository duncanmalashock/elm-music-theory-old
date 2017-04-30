module Chord exposing (..)

import Note exposing (Note(..))
import Scale exposing (..)
import Key exposing (..)


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


majorChordWithRootAndFactors : Note -> List Int -> Chord
majorChordWithRootAndFactors root factors =
    let
        (Key tonic scale) =
            majorKeyFromTonic root
    in
        List.map (scaleDegree scale) factors


minorChordWithRootAndFactors : Note -> List Int -> Chord
minorChordWithRootAndFactors root factors =
    let
        (Key tonic scale) =
            minorKeyFromTonic root
    in
        List.map (scaleDegree scale) factors
