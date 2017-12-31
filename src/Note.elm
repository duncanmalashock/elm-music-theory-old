module Note
    exposing
        ( Note
        , note
        , noteWithOctave
        , noteWithOctaveAndDuration
        , pitchClassFromNote
        , letterNameFromNote
        , accidentalFromNote
        , toString
        )

import PitchClass
    exposing
        ( PitchClass
        , LetterName(..)
        , Accidental(..)
        , letterNameFromPitchClass
        , accidentalFromPitchClass
        )


type Note
    = Note PitchClass
    | NoteWithOctave PitchClass Octave
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


note : LetterName -> Accidental -> Note
note letterName accidental =
    Note <| pitchClass letterName accidental


pitchClass : LetterName -> Accidental -> PitchClass
pitchClass letterName accidental =
    pitchClass letterName accidental


noteWithOctave : LetterName -> Accidental -> Octave -> Note
noteWithOctave letterName accidental octave =
    NoteWithOctave (pitchClass letterName accidental) octave


noteWithOctaveAndDuration : LetterName -> Accidental -> Octave -> Duration -> Note
noteWithOctaveAndDuration letterName accidental octave duration =
    NoteWithOctaveAndDuration (pitchClass letterName accidental) octave duration


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


pitchClassFromNote : Note -> PitchClass
pitchClassFromNote note =
    case note of
        Note pitchClass ->
            pitchClass

        NoteWithOctave pitchClass _ ->
            pitchClass

        NoteWithOctaveAndDuration pitchClass _ _ ->
            pitchClass


toString : Note -> String
toString note =
    case note of
        Note pitchClass ->
            PitchClass.toString pitchClass

        NoteWithOctave pitchClass octave ->
            PitchClass.toString pitchClass ++ octaveToString octave

        NoteWithOctaveAndDuration pitchClass octave duration ->
            PitchClass.toString pitchClass ++ octaveToString octave ++ " " ++ (durationToString duration)


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
