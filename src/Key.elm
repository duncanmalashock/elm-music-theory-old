module Key
    exposing
        ( Key
        , scaleFromKey
        , majorKeyFromTonic
        , minorKeyFromTonic
        , relativeMajorOfKey
        , relativeMinorOfKey
        , parallelMajorOfKey
        , parallelMinorOfKey
        )

import Note exposing (Note(..))
import Scale exposing (Scale(..))


type Key
    = Key Note Scale


scaleFromKey : Key -> Scale
scaleFromKey (Key tonic scale) =
    scale


relativeKey : Key -> Key
relativeKey (Key tonic scale) =
    case scale of
        Minor degrees ->
            majorKeyFromTonic degrees.thirdDegree

        Major degrees ->
            minorKeyFromTonic degrees.sixthDegree


parallelKey : Key -> Key
parallelKey (Key tonic scale) =
    case scale of
        Major degrees ->
            minorKeyFromTonic tonic

        Minor degrees ->
            majorKeyFromTonic tonic


majorKeyFromTonic : Note -> Key
majorKeyFromTonic tonic =
    case tonic of
        C ->
            Key C
                (Major
                    { firstDegree = C
                    , secondDegree = D
                    , thirdDegree = E
                    , fourthDegree = F
                    , fifthDegree = G
                    , sixthDegree = A
                    , seventhDegree = B
                    }
                )

        CSharp ->
            Key CSharp
                (Major
                    { firstDegree = CSharp
                    , secondDegree = DSharp
                    , thirdDegree = ESharp
                    , fourthDegree = FSharp
                    , fifthDegree = GSharp
                    , sixthDegree = ASharp
                    , seventhDegree = BSharp
                    }
                )

        DFlat ->
            Key DFlat
                (Major
                    { firstDegree = DFlat
                    , secondDegree = EFlat
                    , thirdDegree = F
                    , fourthDegree = GFlat
                    , fifthDegree = AFlat
                    , sixthDegree = BFlat
                    , seventhDegree = C
                    }
                )

        D ->
            Key D
                (Major
                    { firstDegree = D
                    , secondDegree = E
                    , thirdDegree = FSharp
                    , fourthDegree = G
                    , fifthDegree = A
                    , sixthDegree = B
                    , seventhDegree = CSharp
                    }
                )

        EFlat ->
            Key EFlat
                (Major
                    { firstDegree = EFlat
                    , secondDegree = F
                    , thirdDegree = G
                    , fourthDegree = AFlat
                    , fifthDegree = BFlat
                    , sixthDegree = C
                    , seventhDegree = D
                    }
                )

        E ->
            Key E
                (Major
                    { firstDegree = E
                    , secondDegree = FSharp
                    , thirdDegree = GSharp
                    , fourthDegree = A
                    , fifthDegree = B
                    , sixthDegree = CSharp
                    , seventhDegree = DSharp
                    }
                )

        F ->
            Key F
                (Major
                    { firstDegree = F
                    , secondDegree = G
                    , thirdDegree = A
                    , fourthDegree = BFlat
                    , fifthDegree = C
                    , sixthDegree = D
                    , seventhDegree = E
                    }
                )

        FSharp ->
            Key FSharp
                (Major
                    { firstDegree = FSharp
                    , secondDegree = GSharp
                    , thirdDegree = ASharp
                    , fourthDegree = B
                    , fifthDegree = CSharp
                    , sixthDegree = DSharp
                    , seventhDegree = ESharp
                    }
                )

        GFlat ->
            Key GFlat
                (Major
                    { firstDegree = GFlat
                    , secondDegree = AFlat
                    , thirdDegree = BFlat
                    , fourthDegree = CFlat
                    , fifthDegree = DFlat
                    , sixthDegree = EFlat
                    , seventhDegree = F
                    }
                )

        G ->
            Key G
                (Major
                    { firstDegree = G
                    , secondDegree = A
                    , thirdDegree = B
                    , fourthDegree = C
                    , fifthDegree = D
                    , sixthDegree = E
                    , seventhDegree = FSharp
                    }
                )

        AFlat ->
            Key AFlat
                (Major
                    { firstDegree = AFlat
                    , secondDegree = BFlat
                    , thirdDegree = C
                    , fourthDegree = DFlat
                    , fifthDegree = EFlat
                    , sixthDegree = F
                    , seventhDegree = G
                    }
                )

        A ->
            Key A
                (Major
                    { firstDegree = A
                    , secondDegree = B
                    , thirdDegree = CSharp
                    , fourthDegree = D
                    , fifthDegree = E
                    , sixthDegree = FSharp
                    , seventhDegree = GSharp
                    }
                )

        BFlat ->
            Key BFlat
                (Major
                    { firstDegree = BFlat
                    , secondDegree = C
                    , thirdDegree = D
                    , fourthDegree = EFlat
                    , fifthDegree = F
                    , sixthDegree = G
                    , seventhDegree = A
                    }
                )

        B ->
            Key B
                (Major
                    { firstDegree = B
                    , secondDegree = CSharp
                    , thirdDegree = DSharp
                    , fourthDegree = E
                    , fifthDegree = FSharp
                    , sixthDegree = GSharp
                    , seventhDegree = ASharp
                    }
                )

        CFlat ->
            Key CFlat
                (Major
                    { firstDegree = CFlat
                    , secondDegree = DFlat
                    , thirdDegree = EFlat
                    , fourthDegree = FFlat
                    , fifthDegree = GFlat
                    , sixthDegree = AFlat
                    , seventhDegree = BFlat
                    }
                )

        _ ->
            majorKeyFromTonic <| Note.getEnharmonicEquivalent tonic


minorKeyFromTonic : Note -> Key
minorKeyFromTonic tonic =
    case tonic of
        C ->
            Key C
                (Minor
                    { firstDegree = C
                    , secondDegree = D
                    , thirdDegree = EFlat
                    , fourthDegree = F
                    , fifthDegree = G
                    , sixthDegree = AFlat
                    , seventhDegree = BFlat
                    }
                )

        CSharp ->
            Key CSharp
                (Minor
                    { firstDegree = CSharp
                    , secondDegree = DSharp
                    , thirdDegree = E
                    , fourthDegree = FSharp
                    , fifthDegree = GSharp
                    , sixthDegree = A
                    , seventhDegree = B
                    }
                )

        D ->
            Key D
                (Minor
                    { firstDegree = D
                    , secondDegree = E
                    , thirdDegree = F
                    , fourthDegree = G
                    , fifthDegree = A
                    , sixthDegree = BFlat
                    , seventhDegree = C
                    }
                )

        DSharp ->
            Key DSharp
                (Minor
                    { firstDegree = DSharp
                    , secondDegree = ESharp
                    , thirdDegree = FSharp
                    , fourthDegree = GSharp
                    , fifthDegree = ASharp
                    , sixthDegree = B
                    , seventhDegree = CSharp
                    }
                )

        EFlat ->
            Key EFlat
                (Minor
                    { firstDegree = EFlat
                    , secondDegree = F
                    , thirdDegree = GFlat
                    , fourthDegree = AFlat
                    , fifthDegree = BFlat
                    , sixthDegree = CFlat
                    , seventhDegree = DFlat
                    }
                )

        E ->
            Key E
                (Minor
                    { firstDegree = E
                    , secondDegree = FSharp
                    , thirdDegree = G
                    , fourthDegree = A
                    , fifthDegree = B
                    , sixthDegree = C
                    , seventhDegree = D
                    }
                )

        F ->
            Key F
                (Minor
                    { firstDegree = F
                    , secondDegree = G
                    , thirdDegree = AFlat
                    , fourthDegree = BFlat
                    , fifthDegree = C
                    , sixthDegree = DFlat
                    , seventhDegree = EFlat
                    }
                )

        FSharp ->
            Key FSharp
                (Minor
                    { firstDegree = FSharp
                    , secondDegree = GSharp
                    , thirdDegree = A
                    , fourthDegree = B
                    , fifthDegree = CSharp
                    , sixthDegree = D
                    , seventhDegree = E
                    }
                )

        G ->
            Key G
                (Minor
                    { firstDegree = G
                    , secondDegree = A
                    , thirdDegree = BFlat
                    , fourthDegree = C
                    , fifthDegree = D
                    , sixthDegree = EFlat
                    , seventhDegree = F
                    }
                )

        GSharp ->
            Key GSharp
                (Minor
                    { firstDegree = GSharp
                    , secondDegree = ASharp
                    , thirdDegree = B
                    , fourthDegree = CSharp
                    , fifthDegree = DSharp
                    , sixthDegree = E
                    , seventhDegree = FSharp
                    }
                )

        AFlat ->
            Key AFlat
                (Minor
                    { firstDegree = AFlat
                    , secondDegree = BFlat
                    , thirdDegree = CFlat
                    , fourthDegree = DFlat
                    , fifthDegree = EFlat
                    , sixthDegree = FFlat
                    , seventhDegree = GFlat
                    }
                )

        A ->
            Key A
                (Minor
                    { firstDegree = A
                    , secondDegree = B
                    , thirdDegree = C
                    , fourthDegree = D
                    , fifthDegree = E
                    , sixthDegree = F
                    , seventhDegree = G
                    }
                )

        ASharp ->
            Key ASharp
                (Minor
                    { firstDegree = ASharp
                    , secondDegree = BSharp
                    , thirdDegree = CSharp
                    , fourthDegree = DSharp
                    , fifthDegree = ESharp
                    , sixthDegree = FSharp
                    , seventhDegree = GSharp
                    }
                )

        BFlat ->
            Key BFlat
                (Minor
                    { firstDegree = BFlat
                    , secondDegree = C
                    , thirdDegree = DFlat
                    , fourthDegree = EFlat
                    , fifthDegree = F
                    , sixthDegree = GFlat
                    , seventhDegree = AFlat
                    }
                )

        B ->
            Key B
                (Minor
                    { firstDegree = B
                    , secondDegree = CSharp
                    , thirdDegree = D
                    , fourthDegree = E
                    , fifthDegree = FSharp
                    , sixthDegree = G
                    , seventhDegree = A
                    }
                )

        _ ->
            minorKeyFromTonic <| Note.getEnharmonicEquivalent tonic
