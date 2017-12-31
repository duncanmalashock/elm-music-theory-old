module SpellIntervals exposing (getPitchClassAtIntervalFrom)

import PitchClass
    exposing
        ( PitchClass
        , LetterName(..)
        , Accidental(..)
        , pitchClass
        , letterName
        )
import Interval exposing (Interval(..))


getPitchClassAtIntervalFrom : PitchClass -> Interval -> PitchClass
getPitchClassAtIntervalFrom thePitchClass interval =
    let
        startingLetterName =
            PitchClass.letterName thePitchClass

        startingAccidental =
            PitchClass.accidental thePitchClass

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
            accidentalToSemitones <| PitchClass.accidental pitchClass

        noteSemitones =
            case PitchClass.letterName pitchClass of
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
semitonesBetween startPitchClass endPitchClass =
    ((semitonesFromC endPitchClass) - (semitonesFromC startPitchClass)) % 12


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


simplifyAccidental : PitchClass -> PitchClass
simplifyAccidental thePitchClass =
    let
        letterName =
            PitchClass.letterName thePitchClass
    in
        case PitchClass.accidental thePitchClass of
            Natural ->
                pitchClass letterName Natural

            Sharp ->
                case PitchClass.letterName thePitchClass of
                    C ->
                        pitchClass C Sharp

                    D ->
                        pitchClass D Sharp

                    E ->
                        pitchClass F Natural

                    F ->
                        pitchClass F Sharp

                    G ->
                        pitchClass G Sharp

                    A ->
                        pitchClass A Sharp

                    B ->
                        pitchClass C Natural

            Flat ->
                case letterName of
                    C ->
                        pitchClass B Natural

                    D ->
                        pitchClass D Flat

                    E ->
                        pitchClass E Flat

                    F ->
                        pitchClass E Natural

                    G ->
                        pitchClass G Flat

                    A ->
                        pitchClass A Flat

                    B ->
                        pitchClass B Flat

            DoubleSharp ->
                case letterName of
                    C ->
                        pitchClass D Natural

                    D ->
                        pitchClass E Natural

                    E ->
                        pitchClass F Sharp

                    F ->
                        pitchClass G Natural

                    G ->
                        pitchClass A Natural

                    A ->
                        pitchClass B Natural

                    B ->
                        pitchClass C Sharp

            DoubleFlat ->
                case letterName of
                    C ->
                        pitchClass B Flat

                    D ->
                        pitchClass C Natural

                    E ->
                        pitchClass D Natural

                    F ->
                        pitchClass E Flat

                    G ->
                        pitchClass F Natural

                    A ->
                        pitchClass G Natural

                    B ->
                        pitchClass A Natural

            TripleSharp ->
                case letterName of
                    C ->
                        pitchClass D Sharp

                    D ->
                        pitchClass F Natural

                    E ->
                        pitchClass G Natural

                    F ->
                        pitchClass G Sharp

                    G ->
                        pitchClass A Sharp

                    A ->
                        pitchClass C Natural

                    B ->
                        pitchClass D Natural

            TripleFlat ->
                case letterName of
                    C ->
                        pitchClass A Natural

                    D ->
                        pitchClass B Natural

                    E ->
                        pitchClass D Flat

                    F ->
                        pitchClass D Natural

                    G ->
                        pitchClass E Natural

                    A ->
                        pitchClass G Flat

                    B ->
                        pitchClass A Flat
