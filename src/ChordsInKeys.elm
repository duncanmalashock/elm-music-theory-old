module ChordsInKeys
    exposing
        ( keysForChord
        , triadChordAtDegree
        , seventhChordAtDegree
        )

import Key exposing (Key(..), Degree(..))
import Chord exposing (Chord, TriadQuality(..), SeventhQuality(..))
import Scale exposing (HeptatonicScaleIntervals)
import Interval exposing (Interval)
import SpellIntervals


isInKey : Key -> Chord -> Bool
isInKey key chord =
    List.member chord (List.map (seventhChordAtDegree key) [ I, II, III, IV, V, VI, VII ])


keysForChord : Chord -> List Key
keysForChord chord =
    List.filter (\k -> isInKey k chord) Key.allKeys


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
