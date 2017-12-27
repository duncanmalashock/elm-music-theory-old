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
            [ test "returns correct chord for the degree" <|
                \_ ->
                    let
                        dFlatMajor =
                            MajorKey <| Note D Flat

                        chord =
                            Key.chordAtDegree dFlatMajor III
                    in
                        Expect.equal chord <| Chord (Note F Natural) MinorSeven
            ]
        ]
