module Chord exposing (Chord(..), ChordQuality(..), isChordTone)

import Scale exposing (Scale(..))
import Note exposing (Note(..), LetterName(..), Accidental(..))
import SpellIntervals as SpellIntervals exposing (getNoteAtIntervalFrom)


type Chord
    = Chord Note ChordQuality


type ChordQuality
    = MajorSeven
    | MinorSeven
    | DominantSeven
    | MinorSevenFlatFive


isChordTone : Chord -> Note -> Bool
isChordTone chord note =
    List.member note (notesInChord chord)


seventhChordNotes : Scale -> List Note
seventhChordNotes (HeptatonicScale root scale) =
    [ scale.first
    , scale.third
    , scale.fifth
    , scale.seventh
    ]
        |> List.map (getNoteAtIntervalFrom root)


notesInChord : Chord -> List Note
notesInChord (Chord root chordQuality) =
    case chordQuality of
        MajorSeven ->
            (HeptatonicScale root Scale.major)
                |> seventhChordNotes

        MinorSeven ->
            (HeptatonicScale root Scale.dorian)
                |> seventhChordNotes

        DominantSeven ->
            (HeptatonicScale root Scale.mixolydian)
                |> seventhChordNotes

        MinorSevenFlatFive ->
            (HeptatonicScale root Scale.locrian)
                |> seventhChordNotes
