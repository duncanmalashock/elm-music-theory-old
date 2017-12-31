module SpellIntervals exposing (getNoteAtIntervalFrom)

import Note
    exposing
        ( Note
        , note
        , letterNameFromNote
        , accidentalFromNote
        )
import PitchClass
    exposing
        ( PitchClass
        , LetterName(..)
        , Accidental(..)
        , pitchClass
        , letterNameFromPitchClass
        , accidentalFromPitchClass
        )
import Interval as Interval exposing (Interval(..))


getNoteAtIntervalFrom : PitchClass -> Interval -> PitchClass
getNoteAtIntervalFrom theNote interval =
    let
        startingLetterName =
            letterNameFromPitchClass theNote

        startingAccidental =
            accidentalFromPitchClass theNote

        newLetterName =
            letterNameAtIntervalFrom startingLetterName interval

        semitonesInInterval =
            Interval.semitoneDistance interval

        semitonesWithoutAccidental =
            semitonesBetween
                (pitchClass startingLetterName startingAccidental)
                (pitchClass newLetterName Natural)

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
            semitonesToAccidental differenceWithoutAccidental
    in
        pitchClass newLetterName newAccidental


letterNameAtIntervalFrom : LetterName -> Interval -> LetterName
letterNameAtIntervalFrom letterName interval =
    letterNameAtDistance letterName (letterNameDistance interval)


semitonesToAccidental : Int -> Accidental
semitonesToAccidental semitones =
    if semitones == 0 then
        Natural
    else if semitones == 1 then
        Sharp
    else if semitones == -1 then
        Flat
    else if semitones == 2 then
        DoubleSharp
    else if semitones == -2 then
        DoubleFlat
    else if semitones == 3 then
        TripleSharp
    else
        TripleFlat


accidentalToSemitones : Accidental -> Int
accidentalToSemitones accidental =
    case accidental of
        Natural ->
            0

        Sharp ->
            1

        Flat ->
            -1

        DoubleSharp ->
            2

        DoubleFlat ->
            -2

        TripleSharp ->
            3

        TripleFlat ->
            -3


semitonesFromC : PitchClass -> Int
semitonesFromC pitchClass =
    let
        offset =
            accidentalToSemitones <| accidentalFromPitchClass pitchClass

        noteSemitones =
            case letterNameFromPitchClass pitchClass of
                C ->
                    0

                D ->
                    2

                E ->
                    4

                F ->
                    5

                G ->
                    7

                A ->
                    9

                B ->
                    11
    in
        (noteSemitones + offset) % 12


letterNameDistance : Interval -> Int
letterNameDistance interval =
    case interval of
        Unison _ ->
            0

        Second _ ->
            1

        Third _ ->
            2

        Fourth _ ->
            3

        Fifth _ ->
            4

        Sixth _ ->
            5

        Seventh _ ->
            6

        Octave _ ->
            7


semitonesBetween : PitchClass -> PitchClass -> Int
semitonesBetween startNote endNote =
    ((semitonesFromC endNote) - (semitonesFromC startNote)) % 12


letterNameAtDistance : LetterName -> Int -> LetterName
letterNameAtDistance startingLetterName steps =
    let
        nextLetterName : Int -> LetterName -> LetterName
        nextLetterName i noteName =
            if i > 0 then
                case noteName of
                    C ->
                        nextLetterName (i - 1) D

                    D ->
                        nextLetterName (i - 1) E

                    E ->
                        nextLetterName (i - 1) F

                    F ->
                        nextLetterName (i - 1) G

                    G ->
                        nextLetterName (i - 1) A

                    A ->
                        nextLetterName (i - 1) B

                    B ->
                        nextLetterName (i - 1) C
            else
                noteName
    in
        nextLetterName (steps % 8) startingLetterName


simplifyAccidental : PitchClass -> Note
simplifyAccidental pitchClass =
    let
        letterName =
            letterNameFromPitchClass pitchClass
    in
        case accidentalFromPitchClass pitchClass of
            Natural ->
                note letterName Natural

            Sharp ->
                case letterNameFromPitchClass pitchClass of
                    C ->
                        note C Sharp

                    D ->
                        note D Sharp

                    E ->
                        note F Natural

                    F ->
                        note F Sharp

                    G ->
                        note G Sharp

                    A ->
                        note A Sharp

                    B ->
                        note C Natural

            Flat ->
                case letterName of
                    C ->
                        note B Natural

                    D ->
                        note D Flat

                    E ->
                        note E Flat

                    F ->
                        note E Natural

                    G ->
                        note G Flat

                    A ->
                        note A Flat

                    B ->
                        note B Flat

            DoubleSharp ->
                case letterName of
                    C ->
                        note D Natural

                    D ->
                        note E Natural

                    E ->
                        note F Sharp

                    F ->
                        note G Natural

                    G ->
                        note A Natural

                    A ->
                        note B Natural

                    B ->
                        note C Sharp

            DoubleFlat ->
                case letterName of
                    C ->
                        note B Flat

                    D ->
                        note C Natural

                    E ->
                        note D Natural

                    F ->
                        note E Flat

                    G ->
                        note F Natural

                    A ->
                        note G Natural

                    B ->
                        note A Natural

            TripleSharp ->
                case letterName of
                    C ->
                        note D Sharp

                    D ->
                        note F Natural

                    E ->
                        note G Natural

                    F ->
                        note G Sharp

                    G ->
                        note A Sharp

                    A ->
                        note C Natural

                    B ->
                        note D Natural

            TripleFlat ->
                case letterName of
                    C ->
                        note A Natural

                    D ->
                        note B Natural

                    E ->
                        note D Flat

                    F ->
                        note D Natural

                    G ->
                        note E Natural

                    A ->
                        note G Flat

                    B ->
                        note A Flat
