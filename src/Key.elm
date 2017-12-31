module Key
    exposing
        ( Key(..)
        , Degree(..)
        , allKeys
        , keysForChord
        , chordAtDegree
        , accidentalsInKey
        )

import Note exposing (Note, LetterName(..), Accidental(..), note)
import Chord exposing (Chord, ChordQuality(..), chord)
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
    [ MajorKey <| note C Natural
    , MinorKey <| note A Natural
    , MajorKey <| note F Natural
    , MinorKey <| note D Natural
    , MajorKey <| note B Flat
    , MinorKey <| note G Natural
    , MajorKey <| note E Flat
    , MinorKey <| note C Natural
    , MajorKey <| note A Flat
    , MinorKey <| note F Natural
    , MajorKey <| note D Flat
    , MinorKey <| note B Flat
    , MajorKey <| note G Flat
    , MinorKey <| note E Flat
    , MajorKey <| note G Natural
    , MinorKey <| note E Natural
    , MajorKey <| note D Natural
    , MinorKey <| note B Natural
    , MajorKey <| note A Natural
    , MinorKey <| note F Sharp
    , MajorKey <| note E Natural
    , MinorKey <| note C Sharp
    , MajorKey <| note B Natural
    , MinorKey <| note G Sharp
    , MajorKey <| note F Sharp
    , MinorKey <| note D Sharp
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
                chord root chordQuality

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
                chord root chordQuality
