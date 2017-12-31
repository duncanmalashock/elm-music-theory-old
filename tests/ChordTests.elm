module ChordTests exposing (suite)

import Chord exposing (Chord(..), ChordQuality(..))
import Note exposing (Note, LetterName(..), Accidental(..), note)
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
                            Chord (note F Sharp) MajorSeven
                    in
                        Expect.equal (Chord.chord cMajorSeven)
                            [ note F Sharp
                            , note A Sharp
                            , note C Sharp
                            , note E Sharp
                            ]
            ]
        , describe "Chord.isChordTone"
            [ test "returns whether the note is a chord tone" <|
                \_ ->
                    let
                        cMajorSeven =
                            Chord (note F Sharp) MajorSeven

                        e =
                            note E Sharp
                    in
                        Expect.equal (Chord.isChordTone cMajorSeven e) True
            ]
        ]
