module Chord
    exposing
        ( Chord
        , TriadQuality(..)
        , SeventhQuality(..)
        , triad
        , triadFromPitchClass
        , seventh
        , seventhFromPitchClass
        , isChordTone
        , pitchClasses
        , toString
        )

import Scale exposing (Scale(..))
import PitchClass exposing (PitchClass, LetterName, Accidental, pitchClass)
import SpellIntervals


type Chord
    = Triad PitchClass TriadQuality
    | Seventh PitchClass SeventhQuality


type TriadQuality
    = Major
    | Minor
    | Diminished
    | Augmented


type SeventhQuality
    = MajorSeven
    | MinorSeven
    | DominantSeven
    | MinorSevenFlatFive


isChordTone : Chord -> PitchClass -> Bool
isChordTone theChord note =
    List.member note (pitchClasses theChord)


triadNotes : Scale -> List PitchClass
triadNotes (HeptatonicScale root scale) =
    [ scale.first
    , scale.third
    , scale.fifth
    ]
        |> List.map (SpellIntervals.getPitchClassAtIntervalFrom root)


seventhNotes : Scale -> List PitchClass
seventhNotes (HeptatonicScale root scale) =
    [ scale.first
    , scale.third
    , scale.fifth
    , scale.seventh
    ]
        |> List.map (SpellIntervals.getPitchClassAtIntervalFrom root)


rootFromChord : Chord -> PitchClass
rootFromChord chord =
    case chord of
        Triad root _ ->
            root

        Seventh root _ ->
            root


triad : LetterName -> Accidental -> TriadQuality -> Chord
triad letterName accidental quality =
    Triad (pitchClass letterName accidental) quality


triadFromPitchClass : PitchClass -> TriadQuality -> Chord
triadFromPitchClass root quality =
    Triad root quality


seventh : LetterName -> Accidental -> SeventhQuality -> Chord
seventh letterName accidental quality =
    Seventh (pitchClass letterName accidental) quality


seventhFromPitchClass : PitchClass -> SeventhQuality -> Chord
seventhFromPitchClass root quality =
    Seventh root quality


pitchClasses : Chord -> List PitchClass
pitchClasses chord =
    let
        notesFn =
            case chord of
                Triad pitchClass triadQuality ->
                    triadNotes

                Seventh pitchClass seventhQuality ->
                    seventhNotes

        scale =
            case chord of
                Seventh root quality ->
                    case quality of
                        MajorSeven ->
                            HeptatonicScale root Scale.major

                        MinorSeven ->
                            HeptatonicScale root Scale.dorian

                        DominantSeven ->
                            HeptatonicScale root Scale.mixolydian

                        MinorSevenFlatFive ->
                            HeptatonicScale root Scale.locrian

                Triad root quality ->
                    case quality of
                        Major ->
                            HeptatonicScale root Scale.major

                        Minor ->
                            HeptatonicScale root Scale.minor

                        Diminished ->
                            HeptatonicScale root Scale.locrian

                        Augmented ->
                            HeptatonicScale root Scale.major
    in
        notesFn scale


toString : Chord -> String
toString chord =
    let
        root =
            case chord of
                Triad root _ ->
                    root

                Seventh root _ ->
                    root

        symbol =
            case chord of
                Triad _ triadQuality ->
                    case triadQuality of
                        Major ->
                            "M"

                        Minor ->
                            "m"

                        Diminished ->
                            "°"

                        Augmented ->
                            "+"

                Seventh _ seventhQuality ->
                    case seventhQuality of
                        MajorSeven ->
                            "Δ"

                        MinorSeven ->
                            "-7"

                        DominantSeven ->
                            "7"

                        MinorSevenFlatFive ->
                            "-7♭5"
    in
        PitchClass.toString root ++ symbol
