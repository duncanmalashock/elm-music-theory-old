module Note
    exposing
        ( Note(..)
        , NoteName(..)
        , Accidental(..)
        , noteNameStepsAway
        , semitonesBetween
        , semitonesToAccidental
        , simplifyAccidental
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
    | Error


type Note
    = Note NoteName Accidental


type alias Octave =
    Int


type NoteOnStaff
    = NoteOnStaff NoteName Accidental Octave


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

        Error ->
            50


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
    else if semitones == -3 then
        TripleFlat
    else
        Error


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

        Error ->
            Note E Error


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
