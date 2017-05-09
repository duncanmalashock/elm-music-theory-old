module Note
    exposing
        ( Note(..)
        , NoteName(..)
        , Accidental(..)
        , noteNameStepsAway
        , semitonesBetween
        , semitonesToAccidental
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
    | TripleSharp
    | TripleFlat


type Note
    = Note NoteName Accidental


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


semitonesBetween : Note -> Note -> Int
semitonesBetween startNote endNote =
    ((semitonesFromC endNote) - (semitonesFromC startNote)) % 12


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
