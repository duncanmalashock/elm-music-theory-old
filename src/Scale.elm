module Scale
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
        , notesInScale
        , isInScale
        , Scale(..)
        , HeptatonicScaleIntervals
        )

import SpellIntervals as SpellIntervals exposing (getNoteAtIntervalFrom)
import Note as Note exposing (Note(..))
import Interval as Interval
    exposing
        ( Interval(..)
        , IntervalName(..)
        , interval
        )


type Scale
    = HeptatonicScale Note HeptatonicScaleIntervals


type alias HeptatonicScaleIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    , seventh : Interval
    }


major : HeptatonicScaleIntervals
major =
    ionian


minor : HeptatonicScaleIntervals
minor =
    aeolian


lydian : HeptatonicScaleIntervals
lydian =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MajorThird
    , fourth = interval AugmentedFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MajorSeventh
    }


ionian : HeptatonicScaleIntervals
ionian =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MajorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MajorSeventh
    }


mixolydian : HeptatonicScaleIntervals
mixolydian =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MajorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MinorSeventh
    }


dorian : HeptatonicScaleIntervals
dorian =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MinorSeventh
    }


aeolian : HeptatonicScaleIntervals
aeolian =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MinorSixth
    , seventh = interval MinorSeventh
    }


phrygian : HeptatonicScaleIntervals
phrygian =
    { first = interval PerfectUnison
    , second = interval MinorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MinorSixth
    , seventh = interval MinorSeventh
    }


locrian : HeptatonicScaleIntervals
locrian =
    { first = interval PerfectUnison
    , second = interval MinorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval DiminishedFifth
    , sixth = interval MinorSixth
    , seventh = interval MinorSeventh
    }


isInScale : Note -> Scale -> Bool
isInScale note scale =
    List.member note (notesInScale scale)


notesInScale : Scale -> List Note
notesInScale (HeptatonicScale root scaleIntervals) =
    List.map (getNoteAtIntervalFrom root) <| listFromHeptatonicScale scaleIntervals


listFromHeptatonicScale : HeptatonicScaleIntervals -> List Interval
listFromHeptatonicScale scale =
    [ scale.first
    , scale.second
    , scale.third
    , scale.fourth
    , scale.fifth
    , scale.sixth
    , scale.seventh
    ]
