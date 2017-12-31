module ChordTests exposing (suite)

import Chord exposing (Chord, ChordQuality(..), chord)
import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Test exposing (..)
import Expect
import Helpers as Expect


suite : Test
suite =
    describe "Chord module"
        [ describe "Chord.pitchClasses"
            [ test "returns the notes of a chord" <|
                \_ ->
                    let
                        cMajorSeven =
                            chord F Sharp MajorSeven
                    in
                        Expect.pitchClassListsEqual
                            (Chord.pitchClasses cMajorSeven)
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
                            chord F Sharp MajorSeven

                        e =
                            pitchClass E Sharp
                    in
                        Expect.equal (Chord.isChordTone cMajorSeven e) True
            ]
        ]
