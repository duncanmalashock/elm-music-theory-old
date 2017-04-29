module Note
    exposing
        ( Note(..)
        , isEnharmonicEquivalent
        , getEnharmonicEquivalent
        )

import ListHelpers


type Note
    = C
    | CSharp
    | DFlat
    | D
    | DSharp
    | EFlat
    | E
    | ESharp
    | FFlat
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
    | BSharp
    | CFlat


enharmonicEquivalentPairs : List (List Note)
enharmonicEquivalentPairs =
    [ [ BSharp, C ]
    , [ CSharp, DFlat ]
    , [ DSharp, EFlat ]
    , [ E, FFlat ]
    , [ ESharp, F ]
    , [ FSharp, GFlat ]
    , [ GSharp, AFlat ]
    , [ ASharp, BFlat ]
    , [ B, CFlat ]
    ]


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


isEnharmonicEquivalent : Note -> Note -> Bool
isEnharmonicEquivalent noteOne noteTwo =
    List.map
        (ListHelpers.isSameList [ noteOne, noteTwo ])
        enharmonicEquivalentPairs
        |> List.member True
