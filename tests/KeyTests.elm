module KeyTests exposing (suite)

import Key exposing (Key(..), Degree(..))
import Chord exposing (Chord(..), ChordQuality(..))
import Note exposing (Note, LetterName(..), Accidental(..), note)
import Test exposing (..)
import Expect


suite : Test
suite =
    describe "Key module"
        [ describe "Key.chordAtDegree"
            [ test "returns correct chords for the degrees" <|
                \_ ->
                    let
                        dFlatMajor =
                            MajorKey <| note D Flat

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
                        Expect.equal chords
                            [ Chord (note D Flat) MajorSeven
                            , Chord (note E Flat) MinorSeven
                            , Chord (note F Natural) MinorSeven
                            , Chord (note G Flat) MajorSeven
                            , Chord (note A Flat) DominantSeven
                            , Chord (note B Flat) MinorSeven
                            , Chord (note C Natural) MinorSevenFlatFive
                            ]
            ]
        , describe "Key.keysForChord"
            [ test "returns the keys a chord belongs to" <|
                \_ ->
                    let
                        keys =
                            Chord (note C Natural) MajorSeven
                                |> Key.keysForChord
                    in
                        Expect.equal keys
                            [ MajorKey (note C Natural)
                            , MinorKey (note A Natural)
                            , MajorKey (note G Natural)
                            , MinorKey (note E Natural)
                            ]
            ]
        , describe "Key.accidentalsInKey"
            [ test "returns the sharp or flat notes in a key" <|
                \_ ->
                    let
                        accidentals =
                            MajorKey (note B Flat)
                                |> Key.accidentalsInKey
                    in
                        Expect.equal accidentals
                            [ note B Flat
                            , note E Flat
                            ]
            ]
        ]
