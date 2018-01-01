module Scale
    exposing
        ( major
        , minor
        , intervals
        , lydian
        , ionian
        , mixolydian
        , dorian
        , aeolian
        , phrygian
        , locrian
        , wholeTone
        , pitchClassesInScale
        , isInScale
        , Scale(..)
        , ScaleDegree(..)
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
    = HexatonicScale PitchClass HexatonicScaleIntervals
    | HeptatonicScale PitchClass HeptatonicScaleIntervals


type ScaleDegree
    = I
    | II
    | III
    | IV
    | V
    | VI
    | VII


type Intervals
    = HexatonicIntervals HexatonicScaleIntervals
    | HeptatonicIntervals HeptatonicScaleIntervals


type alias HexatonicScaleIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    }


type alias HeptatonicScaleIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    , seventh : Interval
    }


major : PitchClass -> Scale
major root =
    ionian root


minor : PitchClass -> Scale
minor root =
    aeolian root


intervals : Scale -> Intervals
intervals scale =
    case scale of
        HeptatonicScale _ intervals ->
            HeptatonicIntervals intervals

        HexatonicScale _ intervals ->
            HexatonicIntervals intervals


lydian : PitchClass -> Scale
lydian root =
    HeptatonicScale root
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MajorThird
        , fourth = interval AugmentedFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MajorSeventh
        }


ionian : PitchClass -> Scale
ionian root =
    HeptatonicScale root
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MajorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MajorSeventh
        }


mixolydian : PitchClass -> Scale
mixolydian root =
    HeptatonicScale root
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MajorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MinorSeventh
        }


dorian : PitchClass -> Scale
dorian root =
    HeptatonicScale root
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MajorSixth
        , seventh = interval MinorSeventh
        }


aeolian : PitchClass -> Scale
aeolian root =
    HeptatonicScale root
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MinorSixth
        , seventh = interval MinorSeventh
        }


phrygian : PitchClass -> Scale
phrygian root =
    HeptatonicScale root
        { first = interval PerfectUnison
        , second = interval MinorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval PerfectFifth
        , sixth = interval MinorSixth
        , seventh = interval MinorSeventh
        }


locrian : PitchClass -> Scale
locrian root =
    HeptatonicScale root
        { first = interval PerfectUnison
        , second = interval MinorSecond
        , third = interval MinorThird
        , fourth = interval PerfectFourth
        , fifth = interval DiminishedFifth
        , sixth = interval MinorSixth
        , seventh = interval MinorSeventh
        }


wholeTone : PitchClass -> Scale
wholeTone root =
    HexatonicScale root
        { first = interval PerfectUnison
        , second = interval MajorSecond
        , third = interval MajorThird
        , fourth = interval AugmentedFourth
        , fifth = interval AugmentedFifth
        , sixth = interval AugmentedSixth
        }


isInScale : PitchClass -> Scale -> Bool
isInScale pitchClass scale =
    List.member pitchClass (pitchClassesInScale scale)


pitchClassesInScale : Scale -> List PitchClass
pitchClassesInScale scale =
    case scale of
        HexatonicScale root intervals ->
            List.map (SpellIntervals.getPitchClassAtIntervalFrom root) <| listFromHexatonicScale intervals

        HeptatonicScale root intervals ->
            List.map (SpellIntervals.getPitchClassAtIntervalFrom root) <| listFromHeptatonicScale intervals


listFromHexatonicScale : HexatonicScaleIntervals -> List Interval
listFromHexatonicScale scale =
    [ scale.first
    , scale.second
    , scale.third
    , scale.fourth
    , scale.fifth
    , scale.sixth
    ]


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
