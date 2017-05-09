module Scale
    exposing
        ( scaleFromIntervals
        , majorScale
        , minorScale
        )

import SpellIntervals exposing (getNoteAtIntervalFrom)
import Note
    exposing
        ( Note(..)
        , NoteName(..)
        , Accidental(..)
        )
import Interval
    exposing
        ( Interval(..)
        , IntervalName(..)
        , interval
        )


majorScale : List Interval
majorScale =
    [ interval PerfectUnison
    , interval MajorSecond
    , interval MajorThird
    , interval PerfectFourth
    , interval PerfectFifth
    , interval MajorSixth
    , interval MajorSeventh
    ]


minorScale : List Interval
minorScale =
    [ interval PerfectUnison
    , interval MajorSecond
    , interval MinorThird
    , interval PerfectFourth
    , interval PerfectFifth
    , interval MinorSixth
    , interval MinorSeventh
    ]


scaleFromIntervals : List Interval -> Note -> List Note
scaleFromIntervals intervals tonic =
    intervals
        |> List.map (getNoteAtIntervalFrom tonic)
