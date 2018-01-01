module ChordsInKeys
    exposing
        ( keysForChord
        , triadChordAtDegree
        , seventhChordAtDegree
        )

import Key exposing (Key(..))
import Chord exposing (Chord(..), TriadQuality(..), SeventhQuality(..))
import Scale exposing (Scale, ScaleDegree(..))
import Interval exposing (Interval)
import SpellIntervals


isInKey : Key -> Chord -> Bool
isInKey key chord =
    case chord of
        Triad _ triadQuality ->
            List.member chord (List.map (triadChordAtDegree key) [ I, II, III, IV, V, VI, VII ])

        Seventh _ seventhQuality ->
            List.member chord (List.map (seventhChordAtDegree key) [ I, II, III, IV, V, VI, VII ])


keysForChord : Chord -> List Key
keysForChord chord =
    List.filter (\k -> isInKey k chord) Key.allKeys


degreeToScaleInterval : Scale -> ScaleDegree -> Interval
degreeToScaleInterval scale degree =
    case scale of
        Scale.HexatonicScale _ intervals ->
            case degree of
                I ->
                    intervals.first

                II ->
                    intervals.second

                III ->
                    intervals.third

                IV ->
                    intervals.fourth

                V ->
                    intervals.fifth

                VI ->
                    intervals.sixth

                VII ->
                    intervals.first

        Scale.HeptatonicScale _ intervals ->
            case degree of
                I ->
                    intervals.first

                II ->
                    intervals.second

                III ->
                    intervals.third

                IV ->
                    intervals.fourth

                V ->
                    intervals.fifth

                VI ->
                    intervals.sixth

                VII ->
                    intervals.seventh


triadChordAtDegree : Key -> ScaleDegree -> Chord
triadChordAtDegree key degree =
    case key of
        MajorKey tonic ->
            let
                scale =
                    Scale.major tonic

                root =
                    SpellIntervals.getPitchClassAtIntervalFrom tonic (degreeToScaleInterval scale degree)

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
                scale =
                    Scale.minor tonic

                root =
                    SpellIntervals.getPitchClassAtIntervalFrom tonic (degreeToScaleInterval scale degree)

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


seventhChordAtDegree : Key -> ScaleDegree -> Chord
seventhChordAtDegree key degree =
    case key of
        MajorKey tonic ->
            let
                scale =
                    Scale.major tonic

                root =
                    SpellIntervals.getPitchClassAtIntervalFrom tonic (degreeToScaleInterval scale degree)

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
                scale =
                    Scale.minor tonic

                root =
                    SpellIntervals.getPitchClassAtIntervalFrom tonic (degreeToScaleInterval scale degree)

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
