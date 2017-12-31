module Key
    exposing
        ( Key
        , majorKey
        , minorKey
        , allKeys
        , Degree(..)
        , keysForChord
        , chordAtDegree
        , accidentalsInKey
        , toString
        )

import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Chord exposing (Chord, ChordQuality(..), chord, chordFromPitchClass)
import Scale exposing (HeptatonicScaleIntervals)
import Interval exposing (Interval)
import SpellIntervals


type Key
    = MajorKey PitchClass
    | MinorKey PitchClass


type Degree
    = I
    | II
    | III
    | IV
    | V
    | VI
    | VII


majorKey : PitchClass -> Key
majorKey pitchClass =
    MajorKey pitchClass


minorKey : PitchClass -> Key
minorKey pitchClass =
    MinorKey pitchClass


allKeys : List Key
allKeys =
    [ MajorKey <| pitchClass C Natural
    , MinorKey <| pitchClass A Natural
    , MajorKey <| pitchClass F Natural
    , MinorKey <| pitchClass D Natural
    , MajorKey <| pitchClass B Flat
    , MinorKey <| pitchClass G Natural
    , MajorKey <| pitchClass E Flat
    , MinorKey <| pitchClass C Natural
    , MajorKey <| pitchClass A Flat
    , MinorKey <| pitchClass F Natural
    , MajorKey <| pitchClass D Flat
    , MinorKey <| pitchClass B Flat
    , MajorKey <| pitchClass G Flat
    , MinorKey <| pitchClass E Flat
    , MajorKey <| pitchClass G Natural
    , MinorKey <| pitchClass E Natural
    , MajorKey <| pitchClass D Natural
    , MinorKey <| pitchClass B Natural
    , MajorKey <| pitchClass A Natural
    , MinorKey <| pitchClass F Sharp
    , MajorKey <| pitchClass E Natural
    , MinorKey <| pitchClass C Sharp
    , MajorKey <| pitchClass B Natural
    , MinorKey <| pitchClass G Sharp
    , MajorKey <| pitchClass F Sharp
    , MinorKey <| pitchClass D Sharp
    ]


accidentalsInKey : Key -> List PitchClass
accidentalsInKey key =
    let
        notes =
            case key of
                MajorKey tonic ->
                    Scale.notesInScale <| Scale.HeptatonicScale tonic (Scale.major)

                MinorKey tonic ->
                    Scale.notesInScale <| Scale.HeptatonicScale tonic (Scale.minor)
    in
        List.filter (\note -> not <| PitchClass.isNatural note) notes


isInKey : Key -> Chord -> Bool
isInKey key chord =
    List.member chord (List.map (chordAtDegree key) [ I, II, III, IV, V, VI, VII ])


keysForChord : Chord -> List Key
keysForChord chord =
    List.filter (\k -> isInKey k chord) allKeys


degreeToHeptatonicScaleInterval : HeptatonicScaleIntervals -> Degree -> Interval
degreeToHeptatonicScaleInterval scaleIntervals degree =
    case degree of
        I ->
            scaleIntervals.first

        II ->
            scaleIntervals.second

        III ->
            scaleIntervals.third

        IV ->
            scaleIntervals.fourth

        V ->
            scaleIntervals.fifth

        VI ->
            scaleIntervals.sixth

        VII ->
            scaleIntervals.seventh


chordAtDegree : Key -> Degree -> Chord
chordAtDegree key degree =
    case key of
        MajorKey tonic ->
            let
                intervals =
                    Scale.major

                root =
                    SpellIntervals.getNoteAtIntervalFrom tonic (degreeToHeptatonicScaleInterval intervals degree)

                chordQuality =
                    case degree of
                        I ->
                            MajorSeven

                        II ->
                            MinorSeven

                        III ->
                            MinorSeven

                        IV ->
                            MajorSeven

                        V ->
                            DominantSeven

                        VI ->
                            MinorSeven

                        VII ->
                            MinorSevenFlatFive
            in
                chordFromPitchClass root chordQuality

        MinorKey tonic ->
            let
                intervals =
                    Scale.minor

                root =
                    SpellIntervals.getNoteAtIntervalFrom tonic (degreeToHeptatonicScaleInterval intervals degree)

                chordQuality =
                    case degree of
                        I ->
                            MinorSeven

                        II ->
                            MinorSevenFlatFive

                        III ->
                            MajorSeven

                        IV ->
                            MinorSeven

                        V ->
                            MinorSeven

                        VI ->
                            MajorSeven

                        VII ->
                            DominantSeven
            in
                chordFromPitchClass root chordQuality


toString : Key -> String
toString key =
    case key of
        MajorKey root ->
            PitchClass.toString root ++ " major"

        MinorKey root ->
            PitchClass.toString root ++ " minor"
