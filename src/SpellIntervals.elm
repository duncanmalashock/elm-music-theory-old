module SpellIntervals
    exposing
        ( getNoteAtIntervalFrom
        )

import Note as Note
    exposing
        ( Note(..)
        , LetterName(..)
        , Accidental(..)
        )
import Interval as Interval
    exposing
        ( Interval
        )


letterNameAtIntervalFrom : LetterName -> Interval -> LetterName
letterNameAtIntervalFrom letterName interval =
    Note.letterNameAtDistance letterName (Interval.letterNameDistance interval)


getNoteAtIntervalFrom : Note -> Interval -> Note
getNoteAtIntervalFrom (Note letterName accidental) interval =
    let
        startNote =
            Note letterName accidental

        newLetterName =
            letterNameAtIntervalFrom letterName interval

        semitonesInInterval =
            Interval.semitones interval

        newNoteWithoutAccidentals =
            Note newLetterName Natural

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
        Note newLetterName newAccidental
