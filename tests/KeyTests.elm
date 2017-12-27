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
        ]
