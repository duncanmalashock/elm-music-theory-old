module Note
    exposing
        ( Note(..)
        , enharmonicEquivalentPairs
        , getEnharmonicEquivalent
        )

import Dict exposing (Dict)
import ListHelpers


type Note
    = C
    | CSharp
    | DFlat
    | D
    | DSharp
    | EFlat
    | E
    | FFlat
    | ESharp
    | F
    | FSharp
    | GFlat
    | G
    | GSharp
    | AFlat
    | A
    | ASharp
    | BFlat
    | B
    | CFlat
    | BSharp


notesByDistanceFromC : Dict Int (List Note)
notesByDistanceFromC =
    Dict.fromList
        [ ( 0, [ C, BSharp ] )
        , ( 1, [ CSharp, DFlat ] )
        , ( 2, [ D ] )
        , ( 3, [ DSharp, EFlat ] )
        , ( 4, [ E, FFlat ] )
        , ( 5, [ F, ESharp ] )
        , ( 6, [ FSharp, GFlat ] )
        , ( 7, [ G ] )
        , ( 8, [ GSharp, AFlat ] )
        , ( 9, [ A ] )
        , ( 10, [ ASharp, BFlat ] )
        , ( 11, [ B, CFlat ] )
        ]


enharmonicEquivalentPairs : List (List Note)
enharmonicEquivalentPairs =
    Dict.values notesByDistanceFromC
        |> List.filter (\l -> (List.length l) > 1)


getEnharmonicEquivalent : Note -> Note
getEnharmonicEquivalent note =
    Maybe.withDefault C
        ((List.filter
            (List.member note)
            enharmonicEquivalentPairs
         )
            |> List.head
            |> Maybe.withDefault [ C ]
            |> List.filter (\n -> n /= note)
            |> List.head
        )
