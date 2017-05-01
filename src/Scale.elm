module Scale
    exposing
        ( Scale(..)
        , majorScaleFromTonic
        , minorScaleFromTonic
        , isInScale
        , degree
        )

import Note exposing (Note(..))
import Interval exposing (..)


type alias ScaleDegrees =
    List Note


type Scale
    = Major ScaleDegrees
    | Minor ScaleDegrees


isInScale : Scale -> Note -> Bool
isInScale scale note =
    case scale of
        Major scaleDegrees ->
            List.member note scaleDegrees

        Minor scaleDegrees ->
            List.member note scaleDegrees


chooseNoteInScale : Scale -> List Note -> Note
chooseNoteInScale scale noteOptions =
    List.filter (\n -> (isInScale scale n)) noteOptions
        |> List.head
        |> Maybe.withDefault C


degree : Scale -> Interval -> Note
degree scale interval =
    case scale of
        Major scaleDegrees ->
            let
                intervalsInScale =
                    [ PerfectUnison
                    , MajorSecond
                    , MajorThird
                    , PerfectFourth
                    , PerfectFifth
                    , MajorSixth
                    , MajorSeventh
                    ]
            in
                case (List.member interval intervalsInScale) of
                    True ->
                        getNoteByInterval (tonic scale) interval
                            |> chooseNoteInScale scale

                    False ->
                        getNoteByInterval (tonic scale) interval
                            |> chooseNoteInScale scale

        Minor scaleDegrees ->
            let
                intervalsInScale =
                    [ PerfectUnison
                    , MajorSecond
                    , MinorThird
                    , PerfectFourth
                    , PerfectFifth
                    , MinorSixth
                    , MinorSeventh
                    ]
            in
                case (List.member interval intervalsInScale) of
                    True ->
                        getNoteByInterval (tonic scale) interval
                            |> chooseNoteInScale scale

                    False ->
                        getNoteByInterval (tonic scale) interval
                            |> chooseNoteInScale scale


tonic : Scale -> Note
tonic scale =
    case scale of
        Major scaleDegrees ->
            Maybe.withDefault C <|
                List.head scaleDegrees

        Minor scaleDegrees ->
            Maybe.withDefault C <|
                List.head scaleDegrees



-- relativeScale : Scale -> Scale
-- relativeScale (scale) =
--     case scale of
--         Minor degrees ->
--             majorScaleFromTonic degrees.thirdDegree
--
--         Major degrees ->
--             minorScaleFromTonic degrees.sixthDegree
--
--
-- parallelScale : Scale -> Scale
-- parallelScale (scale) =
--     case scale of
--         Major degrees ->
--             minorScaleFromTonic tonic
--
--         Minor degrees ->
--             majorScaleFromTonic tonic


majorScaleFromTonic : Note -> Scale
majorScaleFromTonic tonic =
    case tonic of
        C ->
            Major [ C, D, E, F, G, A, B ]

        CSharp ->
            Major [ CSharp, DSharp, ESharp, FSharp, GSharp, ASharp, BSharp ]

        DFlat ->
            Major [ DFlat, EFlat, F, GFlat, AFlat, BFlat, C ]

        D ->
            Major [ D, E, FSharp, G, A, B, CSharp ]

        EFlat ->
            Major [ EFlat, F, G, AFlat, BFlat, C, D ]

        E ->
            Major [ E, FSharp, GSharp, A, B, CSharp, DSharp ]

        F ->
            Major [ F, G, A, BFlat, C, D, E ]

        FSharp ->
            Major [ FSharp, GSharp, ASharp, B, CSharp, DSharp, ESharp ]

        GFlat ->
            Major [ GFlat, AFlat, BFlat, CFlat, DFlat, EFlat, F ]

        G ->
            Major [ G, A, B, C, D, E, FSharp ]

        AFlat ->
            Major [ AFlat, BFlat, C, DFlat, EFlat, F, G ]

        A ->
            Major [ A, B, CSharp, D, E, FSharp, GSharp ]

        BFlat ->
            Major [ BFlat, C, D, EFlat, F, G, A ]

        B ->
            Major [ B, CSharp, DSharp, E, FSharp, GSharp, ASharp ]

        CFlat ->
            Major [ CFlat, DFlat, EFlat, FFlat, GFlat, AFlat, BFlat ]

        _ ->
            majorScaleFromTonic <| Note.getEnharmonicEquivalent tonic


minorScaleFromTonic : Note -> Scale
minorScaleFromTonic tonic =
    case tonic of
        C ->
            Minor [ C, D, EFlat, F, G, AFlat, BFlat ]

        CSharp ->
            Minor [ CSharp, DSharp, E, FSharp, GSharp, A, B ]

        D ->
            Minor [ D, E, F, G, A, BFlat, C ]

        DSharp ->
            Minor [ DSharp, ESharp, FSharp, GSharp, ASharp, B, CSharp ]

        EFlat ->
            Minor [ EFlat, F, GFlat, AFlat, BFlat, CFlat, DFlat ]

        E ->
            Minor [ E, FSharp, G, A, B, C, D ]

        F ->
            Minor [ F, G, AFlat, BFlat, C, DFlat, EFlat ]

        FSharp ->
            Minor [ FSharp, GSharp, A, B, CSharp, D, E ]

        G ->
            Minor [ G, A, BFlat, C, D, EFlat, F ]

        GSharp ->
            Minor [ GSharp, ASharp, B, CSharp, DSharp, E, FSharp ]

        AFlat ->
            Minor [ AFlat, BFlat, CFlat, DFlat, EFlat, FFlat, GFlat ]

        A ->
            Minor [ A, B, C, D, E, F, G ]

        ASharp ->
            Minor [ ASharp, BSharp, CSharp, DSharp, ESharp, FSharp, GSharp ]

        BFlat ->
            Minor [ BFlat, C, DFlat, EFlat, F, GFlat, AFlat ]

        B ->
            Minor [ B, CSharp, D, E, FSharp, G, A ]

        _ ->
            minorScaleFromTonic <| Note.getEnharmonicEquivalent tonic
