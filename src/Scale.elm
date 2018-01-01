module Scale
    exposing
        ( majorIntervals
        , minorIntervals
        , lydianIntervals
        , ionianIntervals
        , mixolydianIntervals
        , dorianIntervals
        , aeolianIntervals
        , phrygianIntervals
        , locrianIntervals
        , pitchClassesInScale
        , isInScale
        , Scale(..)
        , HeptatonicScaleIntervals
        )

import SpellIntervals
import PitchClass exposing (PitchClass)
import Interval
    exposing
        ( Interval(..)
        , IntervalName(..)
        , interval
        )


type Scale
    = HeptatonicScale PitchClass HeptatonicScaleIntervals


type alias HeptatonicScaleIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    , seventh : Interval
    }


majorIntervals : HeptatonicScaleIntervals
majorIntervals =
    ionianIntervals


minorIntervals : HeptatonicScaleIntervals
minorIntervals =
    aeolianIntervals


lydianIntervals : HeptatonicScaleIntervals
lydianIntervals =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MajorThird
    , fourth = interval AugmentedFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MajorSeventh
    }


ionianIntervals : HeptatonicScaleIntervals
ionianIntervals =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MajorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MajorSeventh
    }


mixolydianIntervals : HeptatonicScaleIntervals
mixolydianIntervals =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MajorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MinorSeventh
    }


dorianIntervals : HeptatonicScaleIntervals
dorianIntervals =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MajorSixth
    , seventh = interval MinorSeventh
    }


aeolianIntervals : HeptatonicScaleIntervals
aeolianIntervals =
    { first = interval PerfectUnison
    , second = interval MajorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MinorSixth
    , seventh = interval MinorSeventh
    }


phrygianIntervals : HeptatonicScaleIntervals
phrygianIntervals =
    { first = interval PerfectUnison
    , second = interval MinorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval PerfectFifth
    , sixth = interval MinorSixth
    , seventh = interval MinorSeventh
    }


locrianIntervals : HeptatonicScaleIntervals
locrianIntervals =
    { first = interval PerfectUnison
    , second = interval MinorSecond
    , third = interval MinorThird
    , fourth = interval PerfectFourth
    , fifth = interval DiminishedFifth
    , sixth = interval MinorSixth
    , seventh = interval MinorSeventh
    }


isInScale : PitchClass -> Scale -> Bool
isInScale pitchClass scale =
    List.member pitchClass (pitchClassesInScale scale)


pitchClassesInScale : Scale -> List PitchClass
pitchClassesInScale (HeptatonicScale root scaleIntervals) =
    List.map (SpellIntervals.getPitchClassAtIntervalFrom root) <| listFromHeptatonicScale scaleIntervals


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
