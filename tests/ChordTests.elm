module ChordTests exposing (suite)

import Chord exposing (Chord(..), ChordQuality(..))
import Note exposing (Note(..), LetterName(..), Accidental(..))
import Test exposing (..)
import Expect


suite : Test
suite =
    describe "Chord module"
        [ describe "Chord.chord"
            [ test "returns the notes of a chord" <|
                \_ ->
                    let
                        cMajorSeven =
                            Chord (Note F Sharp) MajorSeven
                    in
                        Expect.equal (Chord.chord cMajorSeven)
                            [ Note F Sharp
                            , Note A Sharp
                            , Note C Sharp
                            , Note E Sharp
                            ]
            ]
        , describe "Chord.isChordTone"
            [ test "returns whether the note is a chord tone" <|
                \_ ->
                    let
                        cMajorSeven =
                            Chord (Note F Sharp) MajorSeven

                        e =
                            Note E Sharp
                    in
                        Expect.equal (Chord.isChordTone cMajorSeven e) True
            ]
        ]
