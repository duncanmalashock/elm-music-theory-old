module ChordTests exposing (suite)

import Chord exposing (Chord, ChordQuality(..), chord, chordNotes)
import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
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
                            chord (pitchClass F Sharp) MajorSeven
                    in
                        Expect.pitchClassListsEqual
                            (chordNotes cMajorSeven)
                            [ pitchClass F Sharp
                            , pitchClass A Sharp
                            , pitchClass C Sharp
                            , pitchClass E Sharp
                            ]
            ]
        , describe "Chord.isChordTone"
            [ test "returns whether the note is a chord tone" <|
                \_ ->
                    let
                        cMajorSeven =
                            chord (pitchClass F Sharp) MajorSeven

                        e =
                            pitchClass E Sharp
                    in
                        Expect.equal (Chord.isChordTone cMajorSeven e) True
            ]
        ]
