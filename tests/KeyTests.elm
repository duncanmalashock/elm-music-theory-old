module KeyTests exposing (suite)

import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Chord exposing (Chord, ChordQuality(..), chord)
import Key exposing (Key(..), Degree(..))
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
                            MajorKey <| pitchClass D Flat

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
                            [ chord (pitchClass D Flat) MajorSeven
                            , chord (pitchClass E Flat) MinorSeven
                            , chord (pitchClass F Natural) MinorSeven
                            , chord (pitchClass G Flat) MajorSeven
                            , chord (pitchClass A Flat) DominantSeven
                            , chord (pitchClass B Flat) MinorSeven
                            , chord (pitchClass C Natural) MinorSevenFlatFive
                            ]
            ]
        , describe "Key.keysForChord"
            [ test "returns the keys a chord belongs to" <|
                \_ ->
                    let
                        keys =
                            chord (pitchClass C Natural) MajorSeven
                                |> Key.keysForChord
                    in
                        Expect.keyListsEqual
                            keys
                            [ MajorKey (pitchClass C Natural)
                            , MinorKey (pitchClass A Natural)
                            , MajorKey (pitchClass G Natural)
                            , MinorKey (pitchClass E Natural)
                            ]
            ]
        , describe "Key.accidentalsInKey"
            [ test "returns the sharp or flat notes in a key" <|
                \_ ->
                    let
                        accidentals =
                            MajorKey (pitchClass B Flat)
                                |> Key.accidentalsInKey
                    in
                        Expect.pitchClassListsEqual
                            accidentals
                            [ pitchClass B Flat
                            , pitchClass E Flat
                            ]
            ]
        ]
