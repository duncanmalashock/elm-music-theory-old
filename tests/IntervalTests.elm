module IntervalTests exposing (suite)

import Interval exposing (IntervalName(..), interval)
import Test exposing (..)
import Expect


suite : Test
suite =
    describe "Interval module"
        [ describe "Interval.semitones"
            [ test "returns semitone value of an interval" <|
                \_ ->
                    let
                        semitonesInAMajorSeventh =
                            Interval.semitones <| interval MajorSeventh
                    in
                        Expect.equal semitonesInAMajorSeventh 11
            ]
        ]
