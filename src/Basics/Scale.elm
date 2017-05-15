module Basics.Scale
    exposing
        ( major
        , minor
        , lydian
        , ionian
        , mixolydian
        , dorian
        , aeolian
        , phrygian
        , locrian
        , degreeOfScale
        , Scale
        )

import Basics.SpellIntervals as SpellIntervals exposing (getNoteAtIntervalFrom)
import Basics.Note as Note
    exposing
        ( Note(..)
        , NoteName(..)
        , Accidental(..)
        )
import Basics.Interval as Interval
    exposing
        ( Interval(..)
        , IntervalName(..)
        , interval
        )


type Scale
    = Lydian HeptatonicScale
    | Ionian HeptatonicScale
    | Mixolydian HeptatonicScale
    | Dorian HeptatonicScale
    | Aeolian HeptatonicScale
    | Phrygian HeptatonicScale
    | Locrian HeptatonicScale


type alias HeptatonicScale =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    , seventh : Interval
    }


major : Scale
major =
    ionian


minor : Scale
minor =
    aeolian


lydian : Scale
lydian =
    Lydian
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MajorThird
        , fourth = interval AugmentedFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MajorSeventh
        }


ionian : Scale
ionian =
    Ionian
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MajorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MajorSeventh
        }


mixolydian : Scale
mixolydian =
    Mixolydian
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MajorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MinorSeventh
        }


dorian : Scale
dorian =
    Dorian
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MinorSeventh
        }


aeolian : Scale
aeolian =
    Aeolian
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MinorSixth
        , seventh = interval MinorSeventh
        }


phrygian : Scale
phrygian =
    Phrygian
        { first = interval PerfectUnison
        , second = interval MinorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MinorSixth
        , seventh = interval MinorSeventh
        }


locrian : Scale
locrian =
    Locrian
        { first = interval PerfectUnison
        , second = interval MinorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval DiminishedFifth
        , sixth = interval MinorSixth
        , seventh = interval MinorSeventh
        }


degreeOfScale : Scale -> Int -> Interval
degreeOfScale scale i =
    case scale of
        Lydian s ->
            degreeOfHeptatonicScale i s

        Ionian s ->
            degreeOfHeptatonicScale i s

        Mixolydian s ->
            degreeOfHeptatonicScale i s

        Dorian s ->
            degreeOfHeptatonicScale i s

        Aeolian s ->
            degreeOfHeptatonicScale i s

        Phrygian s ->
            degreeOfHeptatonicScale i s

        Locrian s ->
            degreeOfHeptatonicScale i s


degreeOfHeptatonicScale : Int -> HeptatonicScale -> Interval
degreeOfHeptatonicScale i scale =
    let
        list =
            listFromHeptatonicScale scale
    in
        Maybe.withDefault (interval PerfectUnison)
            ((List.drop (i % 7) list)
                |> List.head
            )


listFromHeptatonicScale : HeptatonicScale -> List Interval
listFromHeptatonicScale scale =
    [ scale.first
    , scale.second
    , scale.third
    , scale.fourth
    , scale.fifth
    , scale.sixth
    , scale.seventh
    ]
