module Chord exposing (Chord(..), ChordQuality(..), chord, isChordTone)

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
isChordTone theChord note =
    List.member note (chord theChord)


seventhChordNotes : Scale -> List Note
seventhChordNotes (HeptatonicScale root scale) =
    [ scale.first
    , scale.third
    , scale.fifth
    , scale.seventh
    ]
        |> List.map (getNoteAtIntervalFrom root)


chord : Chord -> List Note
chord (Chord root chordQuality) =
    let
        scale =
            case chordQuality of
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
