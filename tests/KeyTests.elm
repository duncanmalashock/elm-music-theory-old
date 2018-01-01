module KeyTests exposing (suite)

import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Chord exposing (Chord, TriadQuality(..), SeventhQuality(..))
import Key exposing (Key, Degree(..), majorKey, minorKey)
import Test exposing (..)
import Helpers as Expect


suite : Test
suite =
    describe "Key module"
        [ describe "Key.triadChordAtDegree"
            [ test "returns correct triad chords for the degrees" <|
                \_ ->
                    let
                        cMajor =
                            majorKey <| pitchClass C Natural

                        chords =
                            List.map (Key.triadChordAtDegree cMajor)
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
                            [ Chord.triad C Natural Major
                            , Chord.triad D Natural Minor
                            , Chord.triad E Natural Minor
                            , Chord.triad F Natural Major
                            , Chord.triad G Natural Major
                            , Chord.triad A Natural Minor
                            , Chord.triad B Natural Diminished
                            ]
            ]
        , describe "Key.seventhChordAtDegree"
            [ test "returns correct seventh chords for the degrees" <|
                \_ ->
                    let
                        dFlatMajor =
                            majorKey <| pitchClass D Flat

                        chords =
                            List.map (Key.seventhChordAtDegree dFlatMajor)
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
                            [ Chord.seventh D Flat MajorSeven
                            , Chord.seventh E Flat MinorSeven
                            , Chord.seventh F Natural MinorSeven
                            , Chord.seventh G Flat MajorSeven
                            , Chord.seventh A Flat DominantSeven
                            , Chord.seventh B Flat MinorSeven
                            , Chord.seventh C Natural MinorSevenFlatFive
                            ]
            ]
        , describe "Key.keysForChord"
            [ test "returns the keys a chord belongs to" <|
                \_ ->
                    let
                        keys =
                            Chord.seventh C Natural MajorSeven
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
        , describe "Key.accidentals"
            [ test "returns the sharp or flat notes in a key" <|
                \_ ->
                    let
                        accidentals =
                            majorKey (pitchClass B Flat)
                                |> Key.accidentals
                    in
                        Expect.pitchClassListsEqual
                            accidentals
                            [ pitchClass B Flat
                            , pitchClass E Flat
                            ]
            ]
        ]
