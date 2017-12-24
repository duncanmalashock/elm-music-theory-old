module SpellIntervals
    exposing
        ( getNoteAtIntervalFrom
        )

import Note as Note
    exposing
        ( Note(..)
        , NoteName(..)
        , Accidental(..)
        )
import Interval as Interval
    exposing
        ( Interval
        )


noteNameAtIntervalFrom : NoteName -> Interval -> NoteName
noteNameAtIntervalFrom noteName interval =
    Note.noteNameStepsAway noteName (Interval.noteNameSteps interval)


getNoteAtIntervalFrom : Note -> Interval -> Note
getNoteAtIntervalFrom (Note noteName accidental) interval =
    let
        startNote =
            Note noteName accidental

        newNoteName =
            noteNameAtIntervalFrom noteName interval

        semitonesInInterval =
            Interval.semitones interval

        newNoteWithoutAccidentals =
            Note newNoteName Natural

        semitonesWithoutAccidental =
            Note.semitonesBetween startNote newNoteWithoutAccidentals

        differenceWithoutAccidental =
            let
                adjust : Int -> Int
                adjust x =
                    if x >= 6 then
                        x - 12
                    else if x <= -6 then
                        x + 12
                    else
                        x
            in
                (semitonesInInterval - semitonesWithoutAccidental)
                    |> adjust

        newAccidental =
            Note.semitonesToAccidental differenceWithoutAccidental
    in
        Note newNoteName newAccidental
