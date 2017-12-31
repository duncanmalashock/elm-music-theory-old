module Key
    exposing
        ( Key
        , majorKey
        , minorKey
        , allKeys
        , Degree(..)
        , keysForChord
        , chordAtDegree
        , accidentals
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
    let
        majors =
            [ pitchClass C Natural
            , pitchClass F Natural
            , pitchClass B Flat
            , pitchClass E Flat
            , pitchClass A Flat
            , pitchClass D Flat
            , pitchClass G Flat
            , pitchClass G Natural
            , pitchClass D Natural
            , pitchClass A Natural
            , pitchClass E Natural
            , pitchClass B Natural
            , pitchClass F Sharp
            ]

        minors =
            [ pitchClass A Natural
            , pitchClass D Natural
            , pitchClass G Natural
            , pitchClass C Natural
            , pitchClass F Natural
            , pitchClass B Flat
            , pitchClass E Flat
            , pitchClass E Natural
            , pitchClass B Natural
            , pitchClass F Sharp
            , pitchClass C Sharp
            , pitchClass G Sharp
            , pitchClass D Sharp
            ]
    in
        List.map majorKey majors
            ++ List.map minorKey minors


accidentals : Key -> List PitchClass
accidentals key =
    let
        notes =
            case key of
                MajorKey tonic ->
                    Scale.pitchClassesInScale <| Scale.HeptatonicScale tonic (Scale.major)

                MinorKey tonic ->
                    Scale.pitchClassesInScale <| Scale.HeptatonicScale tonic (Scale.minor)
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
                    SpellIntervals.getPitchClassAtIntervalFrom tonic (degreeToHeptatonicScaleInterval intervals degree)

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
                    SpellIntervals.getPitchClassAtIntervalFrom tonic (degreeToHeptatonicScaleInterval intervals degree)

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
