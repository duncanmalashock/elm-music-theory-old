module Note
    exposing
        ( Note(..)
        , getEnharmonicEquivalent
        , distanceBetween
        , getNoteByIntervalFrom
        , isSharp
        , isFlat
        , isNatural
        , sharpen
        , flatten
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


sharpNotes : List Note
sharpNotes =
    [ CSharp
    , DSharp
    , ESharp
    , FSharp
    , GSharp
    , ASharp
    , BSharp
    ]


flatNotes : List Note
flatNotes =
    [ CFlat
    , DFlat
    , EFlat
    , FFlat
    , GFlat
    , AFlat
    , BFlat
    ]


naturalNotes : List Note
naturalNotes =
    [ C
    , D
    , E
    , F
    , G
    , A
    , B
    ]


isSharp : Note -> Bool
isSharp note =
    List.member note sharpNotes


isFlat : Note -> Bool
isFlat note =
    List.member note flatNotes


isNatural : Note -> Bool
isNatural note =
    List.member note naturalNotes


sharpen : Note -> Note
sharpen note =
    getNoteByIntervalFrom note 1
        |> List.filter (\n -> not (isFlat n))
        |> List.head
        |> Maybe.withDefault C


flatten : Note -> Note
flatten note =
    getNoteByIntervalFrom note -1
        |> List.filter (\n -> not (isSharp n))
        |> List.head
        |> Maybe.withDefault C


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


distanceFromC : Note -> Int
distanceFromC note =
    Dict.toList notesByDistanceFromC
        |> List.filter (\( key, val ) -> List.member note val)
        |> List.head
        |> Maybe.withDefault ( 0, [ C ] )
        |> Tuple.first


getNoteByIntervalFrom : Note -> Int -> List Note
getNoteByIntervalFrom startNote interval =
    let
        index =
            distanceFromC startNote
                |> (+) interval
                |> flip (%) 12
    in
        Maybe.withDefault [ C ] <| Dict.get index notesByDistanceFromC


distanceBetween : Note -> Note -> Int
distanceBetween noteOne noteTwo =
    (distanceFromC noteTwo - distanceFromC noteOne) % 12


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
