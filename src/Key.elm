module Key
    exposing
        ( Key(..)
        , Degree(..)
        , allKeys
        , keysForChord
        , chordAtDegree
        , accidentalsInKey
        )

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


allKeys : List Key
allKeys =
    [ MajorKey <| Note C Natural
    , MinorKey <| Note A Natural
    , MajorKey <| Note F Natural
    , MinorKey <| Note D Natural
    , MajorKey <| Note B Flat
    , MinorKey <| Note G Natural
    , MajorKey <| Note E Flat
    , MinorKey <| Note C Natural
    , MajorKey <| Note A Flat
    , MinorKey <| Note F Natural
    , MajorKey <| Note D Flat
    , MinorKey <| Note B Flat
    , MajorKey <| Note G Flat
    , MinorKey <| Note E Flat
    , MajorKey <| Note G Natural
    , MinorKey <| Note E Natural
    , MajorKey <| Note D Natural
    , MinorKey <| Note B Natural
    , MajorKey <| Note A Natural
    , MinorKey <| Note F Sharp
    , MajorKey <| Note E Natural
    , MinorKey <| Note C Sharp
    , MajorKey <| Note B Natural
    , MinorKey <| Note G Sharp
    , MajorKey <| Note F Sharp
    , MinorKey <| Note D Sharp
    ]


accidentalsInKey : Key -> List Note
accidentalsInKey key =
    let
        notes =
            case key of
                MajorKey tonic ->
                    Scale.notesInScale <| Scale.HeptatonicScale tonic (Scale.major)

                MinorKey tonic ->
                    Scale.notesInScale <| Scale.HeptatonicScale tonic (Scale.minor)
    in
        List.filter (\note -> not <| Note.isNatural note) notes


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
