module Note
    exposing
        ( Note
        , PitchClass
        , LetterName(..)
        , Accidental(..)
        , note
        , noteWithOctave
        , noteWithOctaveAndDuration
        , isNatural
        , pitchClassFromNote
        , letterNameFromNote
        , accidentalFromNote
        , pitchClass
        , letterNameFromPitchClass
        , accidentalFromPitchClass
        , toString
        )


type Note
    = Note PitchClass
    | NoteWithOctave PitchClass Octave
    | NoteWithOctaveAndDuration PitchClass Octave Duration


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


type Octave
    = Octave0
    | Octave1
    | Octave2
    | Octave3
    | Octave4
    | Octave5
    | Octave6
    | Octave7
    | Octave8
    | Octave9


type Duration
    = Duration RhythmicValue RhythmicModifier


type RhythmicValue
    = WholeNote
    | HalfNote
    | QuarterNote
    | EighthNote
    | SixteenthNote
    | ThirtySecondNote


type RhythmicModifier
    = Normal
    | Dotted
    | DoubleDotted


note : LetterName -> Accidental -> Note
note letterName accidental =
    Note (PitchClass letterName accidental)


pitchClass : LetterName -> Accidental -> PitchClass
pitchClass letterName accidental =
    PitchClass letterName accidental


noteWithOctave : LetterName -> Accidental -> Octave -> Note
noteWithOctave letterName accidental octave =
    NoteWithOctave (PitchClass letterName accidental) octave


noteWithOctaveAndDuration : LetterName -> Accidental -> Octave -> Duration -> Note
noteWithOctaveAndDuration letterName accidental octave duration =
    NoteWithOctaveAndDuration (PitchClass letterName accidental) octave duration


letterNameFromNote : Note -> LetterName
letterNameFromNote note =
    case note of
        Note pitchClass ->
            letterNameFromPitchClass pitchClass

        NoteWithOctave pitchClass _ ->
            letterNameFromPitchClass pitchClass

        NoteWithOctaveAndDuration pitchClass _ _ ->
            letterNameFromPitchClass pitchClass


accidentalFromNote : Note -> Accidental
accidentalFromNote note =
    case note of
        Note pitchClass ->
            accidentalFromPitchClass pitchClass

        NoteWithOctave pitchClass _ ->
            accidentalFromPitchClass pitchClass

        NoteWithOctaveAndDuration pitchClass _ _ ->
            accidentalFromPitchClass pitchClass


letterNameFromPitchClass : PitchClass -> LetterName
letterNameFromPitchClass (PitchClass letterName _) =
    letterName


accidentalFromPitchClass : PitchClass -> Accidental
accidentalFromPitchClass (PitchClass _ accidental) =
    accidental


pitchClassFromNote : Note -> PitchClass
pitchClassFromNote note =
    case note of
        Note (PitchClass letterName accidental) ->
            pitchClass letterName accidental

        NoteWithOctave (PitchClass letterName accidental) _ ->
            pitchClass letterName accidental

        NoteWithOctaveAndDuration (PitchClass letterName accidental) _ _ ->
            pitchClass letterName accidental


isNatural : Note -> Bool
isNatural note =
    accidentalFromNote note == Natural


toString : Note -> String
toString note =
    case note of
        Note pitchClass ->
            pitchClassToString pitchClass

        NoteWithOctave pitchClass octave ->
            pitchClassToString pitchClass ++ octaveToString octave

        NoteWithOctaveAndDuration pitchClass octave duration ->
            pitchClassToString pitchClass ++ octaveToString octave ++ " " ++ (durationToString duration)


pitchClassToString : PitchClass -> String
pitchClassToString (PitchClass letterName accidental) =
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


octaveToString : Octave -> String
octaveToString octave =
    case octave of
        Octave0 ->
            "0"

        Octave1 ->
            "1"

        Octave2 ->
            "2"

        Octave3 ->
            "3"

        Octave4 ->
            "4"

        Octave5 ->
            "5"

        Octave6 ->
            "6"

        Octave7 ->
            "7"

        Octave8 ->
            "8"

        Octave9 ->
            "9"


durationToString : Duration -> String
durationToString (Duration value modifier) =
    let
        rhythm =
            case value of
                WholeNote ->
                    "whole"

                HalfNote ->
                    "half"

                QuarterNote ->
                    "quarter"

                EighthNote ->
                    "eigth"

                SixteenthNote ->
                    "sixteenth"

                ThirtySecondNote ->
                    "thirty-second"

        prefix =
            case modifier of
                Normal ->
                    ""

                Dotted ->
                    "dotted "

                DoubleDotted ->
                    "double-dotted "
    in
        prefix ++ rhythm
