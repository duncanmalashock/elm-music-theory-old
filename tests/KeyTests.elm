module KeyTests exposing (suite)

import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Chord exposing (Chord, ChordQuality(..), chord)
import Key exposing (Key, Degree(..), majorKey, minorKey)
import Test exposing (..)
import Helpers as Expect


suite : Test
suite =
    describe "Key module"
        [ describe "Key.chordAtDegree"
            [ test "returns correct chords for the degrees" <|
                \_ ->
                    let
                        dFlatMajor =
                            majorKey <| pitchClass D Flat

                        chords =
                            List.map (Key.chordAtDegree dFlatMajor)
                                [ I
                                , II
                                , III
                                , IV
                                , V
                                , VI
                                , VII
                                ]
                    in
                        Expect.chordListsEqual chords
                            [ chord D Flat MajorSeven
                            , chord E Flat MinorSeven
                            , chord F Natural MinorSeven
                            , chord G Flat MajorSeven
                            , chord A Flat DominantSeven
                            , chord B Flat MinorSeven
                            , chord C Natural MinorSevenFlatFive
                            ]
            ]
        , describe "Key.keysForChord"
            [ test "returns the keys a chord belongs to" <|
                \_ ->
                    let
                        keys =
                            chord C Natural MajorSeven
                                |> Key.keysForChord
                    in
                        Expect.keyListsEqual
                            keys
                            [ majorKey (pitchClass C Natural)
                            , minorKey (pitchClass A Natural)
                            , majorKey (pitchClass G Natural)
                            , minorKey (pitchClass E Natural)
                            ]
            ]
        , describe "Key.accidentalsInKey"
            [ test "returns the sharp or flat notes in a key" <|
                \_ ->
                    let
                        accidentals =
                            majorKey (pitchClass B Flat)
                                |> Key.accidentalsInKey
                    in
                        Expect.pitchClassListsEqual
                            accidentals
                            [ pitchClass B Flat
                            , pitchClass E Flat
                            ]
            ]
        ]
