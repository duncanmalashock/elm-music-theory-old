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
            [ test "returns the pitch classes in a major seventh chord" <|
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
            , test "returns the pitch classes in an augmented chord" <|
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
            [ test "returns whether the pitch class is a chord tone" <|
                \_ ->
                    let
                        fSharpMajorSeven =
                            Chord.seventh F Sharp MajorSeven

                        eSharp =
                            pitchClass E Sharp
                    in
                        Expect.equal (Chord.isChordTone fSharpMajorSeven eSharp) True
            ]
        ]
