module Interval exposing (..)

import Note exposing (..)


type Interval
    = PerfectUnison
    | MinorSecond
    | MajorSecond
    | MinorThird
    | MajorThird
    | PerfectFourth
    | DiminishedFifth
    | PerfectFifth
    | MinorSixth
    | MajorSixth
    | MinorSeventh
    | MajorSeventh


getNoteByInterval : Note -> Interval -> List Note
getNoteByInterval startingNote interval =
    Note.getNoteByIntervalFrom startingNote <| toSemitones interval


toSemitones : Interval -> Int
toSemitones intervalName =
    case intervalName of
        PerfectUnison ->
            0

        MinorSecond ->
            1

        MajorSecond ->
            2

        MinorThird ->
            3

        MajorThird ->
            4

        PerfectFourth ->
            5

        DiminishedFifth ->
            6

        PerfectFifth ->
            7

        MinorSixth ->
            8

        MajorSixth ->
            9

        MinorSeventh ->
            10

        MajorSeventh ->
            11
