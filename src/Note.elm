module Note exposing (Note(..), isEnharmonicEquivalent)

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


isEnharmonicEquivalent : Note -> Note -> Bool
isEnharmonicEquivalent noteOne noteTwo =
    let
        equivalentPairs =
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
    in
        List.map (ListHelpers.isSameList [ noteOne, noteTwo ]) equivalentPairs
            |> List.member True
