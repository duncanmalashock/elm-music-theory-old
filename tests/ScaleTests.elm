module ScaleTests exposing (suite)

import PitchClass exposing (LetterName(..), Accidental(..), pitchClass)
import Scale exposing (Scale(..))
import Test exposing (..)
import Helpers as Expect


suite : Test
suite =
    describe "Scale module"
        [ describe "Scale.pitchClassesInScale"
            [ test "returns notes of a scale specified" <|
                \_ ->
                    let
                        notes =
                            Scale.pitchClassesInScale <| Scale.major <| pitchClass F Sharp

                        expectedNotes =
                            [ pitchClass F Sharp
                            , pitchClass G Sharp
                            , pitchClass A Sharp
                            , pitchClass B Natural
                            , pitchClass C Sharp
                            , pitchClass D Sharp
                            , pitchClass E Sharp
                            ]
                    in
                        Expect.pitchClassListsEqual notes expectedNotes
            ]
        ]
