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
        , ScaleType
        , Scale(..)
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
    = Scale Note ScaleType


type ScaleType
    = Lydian HeptatonicScale
    | Ionian HeptatonicScale
    | Mixolydian HeptatonicScale
    | Dorian HeptatonicScale
    | Aeolian HeptatonicScale
    | Phrygian HeptatonicScale
    | Locrian HeptatonicScale


type ScaleDegree
    = First
    | SharpFirst
    | FlatSecond
    | Second
    | SharpSecond
    | FlatThird
    | Third
    | Fourth
    | SharpFourth
    | FlatFifth
    | Fifth
    | SharpFifth
    | FlatSixth
    | Sixth
    | FlatSeventh
    | Seventh


type alias HeptatonicScale =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    , seventh : Interval
    }


major : ScaleType
major =
    ionian


minor : ScaleType
minor =
    aeolian


lydian : ScaleType
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


ionian : ScaleType
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


mixolydian : ScaleType
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


dorian : ScaleType
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


aeolian : ScaleType
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


phrygian : ScaleType
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


locrian : ScaleType
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


isInScale : Note -> Scale -> Bool
isInScale note scale =
    List.member note (notesInScale scale)


notesInScale : Scale -> List Note
notesInScale (Scale root scaleType) =
    let
        notesInHeptatonicScale s =
            List.map (getNoteAtIntervalFrom root) (listFromHeptatonicScale s)
    in
        case scaleType of
            Lydian s ->
                notesInHeptatonicScale s

            Ionian s ->
                notesInHeptatonicScale s

            Mixolydian s ->
                notesInHeptatonicScale s

            Dorian s ->
                notesInHeptatonicScale s

            Aeolian s ->
                notesInHeptatonicScale s

            Phrygian s ->
                notesInHeptatonicScale s

            Locrian s ->
                notesInHeptatonicScale s


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
