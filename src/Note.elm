module Note
    exposing
        ( Note
        , noteWithOctave
        , noteWithOctaveAndDuration
        , pitchClass
        , letterName
        , accidental
        , toString
        )

import PitchClass
    exposing
        ( PitchClass
        , LetterName(..)
        , Accidental(..)
        )


type Note
    = NoteWithOctave PitchClass Octave
    | NoteWithOctaveAndDuration PitchClass Octave Duration


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


noteWithOctave : LetterName -> Accidental -> Octave -> Note
noteWithOctave letterName accidental octave =
    NoteWithOctave (PitchClass.pitchClass letterName accidental) octave


noteWithOctaveAndDuration : LetterName -> Accidental -> Octave -> Duration -> Note
noteWithOctaveAndDuration letterName accidental octave duration =
    NoteWithOctaveAndDuration (PitchClass.pitchClass letterName accidental) octave duration


letterName : Note -> LetterName
letterName note =
    case note of
        NoteWithOctave myPitchClass _ ->
            PitchClass.letterName myPitchClass

        NoteWithOctaveAndDuration myPitchClass _ _ ->
            PitchClass.letterName myPitchClass


accidental : Note -> Accidental
accidental note =
    case note of
        NoteWithOctave myPitchClass _ ->
            PitchClass.accidental myPitchClass

        NoteWithOctaveAndDuration myPitchClass _ _ ->
            PitchClass.accidental myPitchClass


pitchClass : Note -> PitchClass
pitchClass note =
    case note of
        NoteWithOctave myPitchClass _ ->
            myPitchClass

        NoteWithOctaveAndDuration myPitchClass _ _ ->
            myPitchClass


toString : Note -> String
toString note =
    case note of
        NoteWithOctave myPitchClass octave ->
            PitchClass.toString myPitchClass ++ octaveToString octave

        NoteWithOctaveAndDuration myPitchClass octave duration ->
            PitchClass.toString myPitchClass ++ octaveToString octave ++ " " ++ (durationToString duration)


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
