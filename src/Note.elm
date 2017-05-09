module Note
    exposing
        ( Note(..)
        , NoteName(..)
        , Accidental(..)
        , noteNameStepsAway
        , accidentalSemitones
        )


type NoteName
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type Accidental
    = Natural
    | Sharp
    | Flat
    | DoubleSharp
    | DoubleFlat


type Note
    = Note NoteName Accidental


accidentalSemitones : Accidental -> Int
accidentalSemitones accidental =
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


noteNameStepsAway : NoteName -> Int -> NoteName
noteNameStepsAway startingNoteName steps =
    let
        nextNoteName : Int -> NoteName -> NoteName
        nextNoteName i noteName =
            if i > 0 then
                case noteName of
                    C ->
                        nextNoteName (i - 1) D

                    D ->
                        nextNoteName (i - 1) E

                    E ->
                        nextNoteName (i - 1) F

                    F ->
                        nextNoteName (i - 1) G

                    G ->
                        nextNoteName (i - 1) A

                    A ->
                        nextNoteName (i - 1) B

                    B ->
                        nextNoteName (i - 1) C
            else
                noteName
    in
        nextNoteName (steps % 8) startingNoteName
