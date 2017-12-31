module PitchClass
    exposing
        ( PitchClass
        , LetterName(..)
        , Accidental(..)
        , pitchClass
        , letterNameFromPitchClass
        , accidentalFromPitchClass
        , isNatural
        , toString
        )


type PitchClass
    = PitchClass LetterName Accidental


type LetterName
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


pitchClass : LetterName -> Accidental -> PitchClass
pitchClass letterName accidental =
    PitchClass letterName accidental


letterNameFromPitchClass : PitchClass -> LetterName
letterNameFromPitchClass (PitchClass letterName _) =
    letterName


accidentalFromPitchClass : PitchClass -> Accidental
accidentalFromPitchClass (PitchClass _ accidental) =
    accidental


isNatural : PitchClass -> Bool
isNatural note =
    accidentalFromPitchClass note == Natural


toString : PitchClass -> String
toString (PitchClass letterName accidental) =
    letterNameToString letterName ++ accidentalToString accidental


letterNameToString : LetterName -> String
letterNameToString letterName =
    case letterName of
        C ->
            "C"

        D ->
            "D"

        E ->
            "E"

        F ->
            "F"

        G ->
            "G"

        A ->
            "A"

        B ->
            "B"


accidentalToString : Accidental -> String
accidentalToString accidental =
    case accidental of
        Natural ->
            ""

        Sharp ->
            "♯"

        Flat ->
            "♭"

        DoubleSharp ->
            "𝄪"

        DoubleFlat ->
            "𝄫"

        TripleSharp ->
            "𝄪♯"

        TripleFlat ->
            "𝄫♭"
