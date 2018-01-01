module Chord
    exposing
        ( Chord(..)
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
triadNotes scale =
    let
        ( root, intervals ) =
            case scale of
                HexatonicScale root scale ->
                    ( root
                    , [ scale.first
                      , scale.third
                      , scale.fifth
                      ]
                    )

                HeptatonicScale root scale ->
                    ( root
                    , [ scale.first
                      , scale.third
                      , scale.fifth
                      ]
                    )
    in
        List.map (SpellIntervals.getPitchClassAtIntervalFrom root) intervals


seventhNotes : Scale -> List PitchClass
seventhNotes scale =
    let
        ( root, intervals ) =
            case scale of
                HexatonicScale root intervals ->
                    ( root
                    , [ intervals.first
                      , intervals.third
                      , intervals.fifth
                      , intervals.first
                      ]
                    )

                HeptatonicScale root intervals ->
                    ( root
                    , [ intervals.first
                      , intervals.third
                      , intervals.fifth
                      , intervals.seventh
                      ]
                    )
    in
        List.map (SpellIntervals.getPitchClassAtIntervalFrom root) intervals


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
                            Scale.major root

                        MinorSeven ->
                            Scale.dorian root

                        DominantSeven ->
                            Scale.mixolydian root

                        MinorSevenFlatFive ->
                            Scale.locrian root

                Triad root quality ->
                    case quality of
                        Major ->
                            Scale.major root

                        Minor ->
                            Scale.minor root

                        Diminished ->
                            Scale.locrian root

                        Augmented ->
                            Scale.wholeTone root
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
