module Note
    exposing
        ( Note(..)
        , LetterName(..)
        , Accidental(..)
        , isNatural
        )


type LetterName
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type Accidental
    = Natural
    | Sharp
    | Flat
    | DoubleSharp
    | DoubleFlat
    | TripleSharp
    | TripleFlat


type Note
    = Note LetterName Accidental


isNatural : Note -> Bool
isNatural (Note letterName accidental) =
    accidental == Natural
