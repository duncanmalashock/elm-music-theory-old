module ChordTests exposing (suite)

import Chord exposing (Chord, ChordQuality(..), chord, chordNotes)
import Note exposing (Note, LetterName(..), Accidental(..), note)
import Test exposing (..)
import Expect
import Helpers as Expect


suite : Test
suite =
    describe "Chord module"
        [ describe "Chord.chord"
            [ test "returns the notes of a chord" <|
                \_ ->
                    let
                        cMajorSeven =
                            chord (note F Sharp) MajorSeven
                    in
                        Expect.listsEqual
                            (chordNotes cMajorSeven)
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
                            chord (note F Sharp) MajorSeven

                        e =
                            note E Sharp
                    in
                        Expect.equal (Chord.isChordTone cMajorSeven e) True
            ]
        ]
