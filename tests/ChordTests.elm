module ChordTests exposing (suite)

import Chord exposing (Chord, TriadQuality(..), SeventhQuality(..))
import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Test exposing (..)
import Expect
import Helpers as Expect


suite : Test
suite =
    describe "Chord module"
        [ describe "Chord.pitchClasses"
            [ test "returns the notes of a major seventh chord" <|
                \_ ->
                    let
                        cMajorSeven =
                            Chord.seventh F Sharp MajorSeven
                    in
                        Expect.pitchClassListsEqual
                            (Chord.pitchClasses cMajorSeven)
                            [ pitchClass F Sharp
                            , pitchClass A Sharp
                            , pitchClass C Sharp
                            , pitchClass E Sharp
                            ]
            , test "returns the notes of an augmented chord" <|
                \_ ->
                    let
                        cAugmented =
                            Chord.triad C Natural Augmented
                    in
                        Expect.pitchClassListsEqual
                            (Chord.pitchClasses cAugmented)
                            [ pitchClass C Natural
                            , pitchClass E Natural
                            , pitchClass G Sharp
                            ]
            ]
        , describe "Chord.isChordTone"
            [ test "returns whether the note is a chord tone" <|
                \_ ->
                    let
                        cMajorSeven =
                            Chord.seventh F Sharp MajorSeven

                        e =
                            pitchClass E Sharp
                    in
                        Expect.equal (Chord.isChordTone cMajorSeven e) True
            ]
        ]
