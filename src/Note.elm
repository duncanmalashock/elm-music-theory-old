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
        )


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


type Duration
    = Duration RhythmicValue RhythmicModifier


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


type PitchClass
    = PitchClass LetterName Accidental


pitchClass : LetterName -> Accidental -> PitchClass
pitchClass letterName accidental =
    PitchClass letterName accidental


type Note
    = Note PitchClass
    | NoteWithOctave PitchClass Octave
    | NoteWithOctaveAndDuration PitchClass Octave Duration


note : LetterName -> Accidental -> Note
note letterName accidental =
    Note (PitchClass letterName accidental)


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
