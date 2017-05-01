module Chord exposing (..)

import Note exposing (Note(..))
import Scale exposing (..)
import Key exposing (..)
import List.Extra


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


toPairs : List a -> List ( a, a )
toPairs lst =
    case lst of
        [] ->
            []

        x :: xs ->
            List.Extra.zip lst (xs ++ [ x ])
                |> List.reverse
                |> List.drop 1
                |> List.reverse


scorePermutationsForRootPosition : Chord -> List ( Chord, Int )
scorePermutationsForRootPosition chord =
    let
        allPermutations =
            List.Extra.permutations chord

        scoringFunction : Chord -> Int
        scoringFunction chord =
            toPairs chord
                |> List.map (\( a, b ) -> Note.distanceBetween a b)
                |> List.map
                    (\interval ->
                        case interval of
                            3 ->
                                0

                            4 ->
                                0

                            _ ->
                                if interval > 4 then
                                    interval - 4
                                else
                                    3 - interval
                    )
                |> List.sum
    in
        List.map (\c -> ( c, scoringFunction c )) allPermutations
            |> List.sortBy (\( p, v ) -> v)
