module KeyTests exposing (suite)

import Key exposing (Key(..), Degree(..))
import Chord exposing (Chord(..), ChordQuality(..))
import Note exposing (Note(..), LetterName(..), Accidental(..))
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
                            MajorKey <| Note D Flat

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
                            [ Chord (Note D Flat) MajorSeven
                            , Chord (Note E Flat) MinorSeven
                            , Chord (Note F Natural) MinorSeven
                            , Chord (Note G Flat) MajorSeven
                            , Chord (Note A Flat) DominantSeven
                            , Chord (Note B Flat) MinorSeven
                            , Chord (Note C Natural) MinorSevenFlatFive
                            ]
            ]
        , describe "Key.keysForChord"
            [ test "returns the keys a chord belongs to" <|
                \_ ->
                    let
                        keys =
                            Chord (Note C Natural) MajorSeven
                                |> Key.keysForChord
                    in
                        Expect.equal keys
                            [ MajorKey (Note C Natural)
                            , MinorKey (Note A Natural)
                            , MajorKey (Note G Natural)
                            , MinorKey (Note E Natural)
                            ]
            ]
        , describe "Key.accidentalsInKey"
            [ test "returns the sharp or flat notes in a key" <|
                \_ ->
                    let
                        accidentals =
                            MajorKey (Note B Flat)
                                |> Key.accidentalsInKey
                    in
                        Expect.equal accidentals
                            [ Note B Flat
                            , Note E Flat
                            ]
            ]
        ]
