module Note
    exposing
        ( Note(..)
        , LetterName(..)
        , Accidental(..)
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
