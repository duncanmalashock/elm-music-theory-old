module Chord
    exposing
        ( Chord
        , ChordQuality(..)
        , chord
        , isChordTone
        , chordNotes
        , toString
        )

import Scale exposing (Scale(..))
import Note exposing (Note, LetterName(..), Accidental(..))
import SpellIntervals as SpellIntervals exposing (getNoteAtIntervalFrom)


type Chord
    = Chord Note ChordQuality


type ChordQuality
    = MajorSeven
    | MinorSeven
    | DominantSeven
    | MinorSevenFlatFive


isChordTone : Chord -> Note -> Bool
isChordTone theChord note =
    List.member note (chordNotes theChord)


seventhChordNotes : Scale -> List Note
seventhChordNotes (HeptatonicScale root scale) =
    [ scale.first
    , scale.third
    , scale.fifth
    , scale.seventh
    ]
        |> List.map (getNoteAtIntervalFrom root)


rootFromChord : Chord -> Note
rootFromChord chord =
    case chord of
        Chord root _ ->
            root


qualityFromChord : Chord -> ChordQuality
qualityFromChord chord =
    case chord of
        Chord _ quality ->
            quality


chord : Note -> ChordQuality -> Chord
chord root quality =
    Chord root quality


chordNotes : Chord -> List Note
chordNotes chord =
    let
        root =
            rootFromChord chord

        scale =
            case qualityFromChord chord of
                MajorSeven ->
                    HeptatonicScale root Scale.major

                MinorSeven ->
                    HeptatonicScale root Scale.dorian

                DominantSeven ->
                    HeptatonicScale root Scale.mixolydian

                MinorSevenFlatFive ->
                    HeptatonicScale root Scale.locrian
    in
        seventhChordNotes scale


toString : Chord -> String
toString (Chord root quality) =
    Note.toString root ++ qualityToString quality


qualityToString : ChordQuality -> String
qualityToString chordQuality =
    case chordQuality of
        MajorSeven ->
            "Δ"

        MinorSeven ->
            "-7"

        DominantSeven ->
            "7"

        MinorSevenFlatFive ->
            "-7♭5"
