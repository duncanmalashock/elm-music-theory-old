module Key exposing (..)

import Note exposing (Note(..), LetterName(..), Accidental(..))
import Chord exposing (Chord(..), ChordQuality(..))
import Scale exposing (HeptatonicScaleIntervals)
import Interval exposing (Interval)
import SpellIntervals


type Key
    = MajorKey Note
    | MinorKey Note


type Degree
    = I
    | II
    | III
    | IV
    | V
    | VI
    | VII


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
                Chord root chordQuality

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
                Chord root chordQuality
