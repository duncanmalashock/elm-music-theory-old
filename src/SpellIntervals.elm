module SpellIntervals exposing (getNoteAtIntervalFrom)

import Note as Note exposing (Note(..), LetterName(..), Accidental(..))
import Interval as Interval exposing (Interval(..))


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


semitonesFromC : Note -> Int
semitonesFromC (Note noteName accidental) =
    let
        offset =
            accidentalToSemitones accidental

        noteSemitones =
            case noteName of
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


semitonesBetween : Note -> Note -> Int
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


getNoteAtIntervalFrom : Note -> Interval -> Note
getNoteAtIntervalFrom (Note letterName accidental) interval =
    let
        startNote =
            Note letterName accidental

        newLetterName =
            letterNameAtIntervalFrom letterName interval

        semitonesInInterval =
            Interval.semitoneDistance interval

        newNoteWithoutAccidentals =
            Note newLetterName Natural

        semitonesWithoutAccidental =
            semitonesBetween startNote newNoteWithoutAccidentals

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
        Note newLetterName newAccidental


simplifyAccidental : Note -> Note
simplifyAccidental (Note noteName accidental) =
    case accidental of
        Natural ->
            Note noteName accidental

        Sharp ->
            case noteName of
                C ->
                    Note C Sharp

                D ->
                    Note D Sharp

                E ->
                    Note F Natural

                F ->
                    Note F Sharp

                G ->
                    Note G Sharp

                A ->
                    Note A Sharp

                B ->
                    Note C Natural

        Flat ->
            case noteName of
                C ->
                    Note B Natural

                D ->
                    Note D Flat

                E ->
                    Note E Flat

                F ->
                    Note E Natural

                G ->
                    Note G Flat

                A ->
                    Note A Flat

                B ->
                    Note B Flat

        DoubleSharp ->
            case noteName of
                C ->
                    Note D Natural

                D ->
                    Note E Natural

                E ->
                    Note F Sharp

                F ->
                    Note G Natural

                G ->
                    Note A Natural

                A ->
                    Note B Natural

                B ->
                    Note C Sharp

        DoubleFlat ->
            case noteName of
                C ->
                    Note B Flat

                D ->
                    Note C Natural

                E ->
                    Note D Natural

                F ->
                    Note E Flat

                G ->
                    Note F Natural

                A ->
                    Note G Natural

                B ->
                    Note A Natural

        TripleSharp ->
            case noteName of
                C ->
                    Note D Sharp

                D ->
                    Note F Natural

                E ->
                    Note G Natural

                F ->
                    Note G Sharp

                G ->
                    Note A Sharp

                A ->
                    Note C Natural

                B ->
                    Note D Natural

        TripleFlat ->
            case noteName of
                C ->
                    Note A Natural

                D ->
                    Note B Natural

                E ->
                    Note D Flat

                F ->
                    Note D Natural

                G ->
                    Note E Natural

                A ->
                    Note G Flat

                B ->
                    Note A Flat
