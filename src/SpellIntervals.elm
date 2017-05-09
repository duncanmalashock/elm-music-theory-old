module SpellIntervals exposing (noteNameAtIntervalFrom)

import Note
    exposing
        ( Note(..)
        , NoteName(..)
        , noteNameStepsAway
        )
import Interval
    exposing
        ( Interval
        , noteNameSteps
        )


noteNameAtIntervalFrom : Interval -> Note -> NoteName
noteNameAtIntervalFrom interval (Note name accidental) =
    noteNameStepsAway name (noteNameSteps interval)
