module Key
    exposing
        ( Key(..)
        , majorKey
        , minorKey
        , allKeys
        , accidentals
        , toString
        )

import PitchClass exposing (PitchClass, LetterName(..), Accidental(..), pitchClass)
import Scale


type Key
    = MajorKey PitchClass
    | MinorKey PitchClass


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
        pitchClasses =
            let
                ( tonic, intervals ) =
                    case key of
                        MajorKey tonic ->
                            ( tonic, Scale.intervals <| Scale.major tonic )

                        MinorKey tonic ->
                            ( tonic, Scale.intervals <| Scale.minor tonic )
            in
                Scale.pitchClassesInScale <| Scale.HeptatonicScale tonic intervals
    in
        List.filter (\pc -> not <| PitchClass.isNatural pc) pitchClasses


toString : Key -> String
toString key =
    case key of
        MajorKey root ->
            PitchClass.toString root ++ " major"

        MinorKey root ->
            PitchClass.toString root ++ " minor"
