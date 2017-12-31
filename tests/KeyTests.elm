module KeyTests exposing (suite)

import Key exposing (Key(..), Degree(..))
import Chord exposing (Chord, ChordQuality(..), chord)
import Note exposing (Note, LetterName(..), Accidental(..), note)
import Test exposing (..)
import Expect
import Helpers as Expect


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
                            [ chord (note D Flat) MajorSeven
                            , chord (note E Flat) MinorSeven
                            , chord (note F Natural) MinorSeven
                            , chord (note G Flat) MajorSeven
                            , chord (note A Flat) DominantSeven
                            , chord (note B Flat) MinorSeven
                            , chord (note C Natural) MinorSevenFlatFive
                            ]
            ]
        , describe "Key.keysForChord"
            [ test "returns the keys a chord belongs to" <|
                \_ ->
                    let
                        keys =
                            chord (note C Natural) MajorSeven
                                |> Key.keysForChord
                    in
                        Expect.listsEqual
                            keys
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
                        Expect.listsEqual
                            accidentals
                            [ note B Flat
                            , note E Flat
                            ]
            ]
        ]
