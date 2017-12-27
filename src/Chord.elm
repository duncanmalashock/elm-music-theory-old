module Chord exposing (..)

import Note exposing (Note(..), LetterName(..), Accidental(..))


type Chord
    = Chord Note ChordQuality


type ChordQuality
    = MajorSeven
    | MinorSeven
    | DominantSeven
    | MinorSevenFlatFive
