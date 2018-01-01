module ChordsInKeysTests exposing (suite)

import ChordsInKeys
import Chord exposing (Chord, TriadQuality(..), SeventhQuality(..))
import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Key exposing (Key, Degree(..), majorKey, minorKey)
import Test exposing (..)
import Helpers as Expect


suite : Test
suite =
    describe "ChordsInKeys module"
        [ describe "ChordsInKeys.triadChordAtDegree"
            [ test "returns correct triad chords for the degrees" <|
                \_ ->
                    let
                        cMajor =
                            majorKey <| pitchClass C Natural

                        chords =
                            List.map (ChordsInKeys.triadChordAtDegree cMajor)
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
        , describe "ChordsInKeys.seventhChordAtDegree"
            [ test "returns correct seventh chords for the degrees" <|
                \_ ->
                    let
                        dFlatMajor =
                            majorKey <| pitchClass D Flat

                        chords =
                            List.map (ChordsInKeys.seventhChordAtDegree dFlatMajor)
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
        , describe "ChordsInKeys.keysForChord"
            [ test "returns the keys a triad chord belongs to" <|
                \_ ->
                    let
                        keys =
                            Chord.triad C Natural Major
                                |> ChordsInKeys.keysForChord
                    in
                        Expect.keyListsEqual
                            keys
                            [ majorKey (pitchClass C Natural)
                            , minorKey (pitchClass A Natural)
                            , majorKey (pitchClass F Natural)
                            , minorKey (pitchClass D Natural)
                            , majorKey (pitchClass G Natural)
                            , minorKey (pitchClass E Natural)
                            ]
            , test "returns the keys a seventh chord belongs to" <|
                \_ ->
                    let
                        keys =
                            Chord.seventh C Natural MajorSeven
                                |> ChordsInKeys.keysForChord
                    in
                        Expect.keyListsEqual
                            keys
                            [ majorKey (pitchClass C Natural)
                            , minorKey (pitchClass A Natural)
                            , majorKey (pitchClass G Natural)
                            , minorKey (pitchClass E Natural)
                            ]
            ]
        ]
