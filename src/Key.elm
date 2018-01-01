module Key
    exposing
        ( Key
        , majorKey
        , minorKey
        , allKeys
        , Degree(..)
        , keysForChord
        , triadChordAtDegree
        , seventhChordAtDegree
        , accidentals
        , toString
        )

import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Chord exposing (Chord, TriadQuality(..), SeventhQuality(..))
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
    List.member chord (List.map (seventhChordAtDegree key) [ I, II, III, IV, V, VI, VII ])


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


triadChordAtDegree : Key -> Degree -> Chord
triadChordAtDegree key degree =
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
                            Major

                        II ->
                            Minor

                        III ->
                            Minor

                        IV ->
                            Major

                        V ->
                            Major

                        VI ->
                            Minor

                        VII ->
                            Diminished
            in
                Chord.triadFromPitchClass root chordQuality

        MinorKey tonic ->
            let
                intervals =
                    Scale.minor

                root =
                    SpellIntervals.getPitchClassAtIntervalFrom tonic (degreeToHeptatonicScaleInterval intervals degree)

                chordQuality =
                    case degree of
                        I ->
                            Minor

                        II ->
                            Diminished

                        III ->
                            Major

                        IV ->
                            Minor

                        V ->
                            Minor

                        VI ->
                            Major

                        VII ->
                            Major
            in
                Chord.triadFromPitchClass root chordQuality


seventhChordAtDegree : Key -> Degree -> Chord
seventhChordAtDegree key degree =
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
                Chord.seventhFromPitchClass root chordQuality

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
                Chord.seventhFromPitchClass root chordQuality


toString : Key -> String
toString key =
    case key of
        MajorKey root ->
            PitchClass.toString root ++ " major"

        MinorKey root ->
            PitchClass.toString root ++ " minor"
