module Scale
    exposing
        ( Scale(..)
        , scaleDegree
        )

import Note exposing (Note(..))


type alias MajorScaleDegrees =
    { firstDegree : Note
    , secondDegree : Note
    , thirdDegree : Note
    , fourthDegree : Note
    , fifthDegree : Note
    , sixthDegree : Note
    , seventhDegree : Note
    }


type alias MinorScaleDegrees =
    { firstDegree : Note
    , secondDegree : Note
    , thirdDegree : Note
    , fourthDegree : Note
    , fifthDegree : Note
    , sixthDegree : Note
    , seventhDegree : Note
    }


type Scale
    = Major MajorScaleDegrees
    | Minor MinorScaleDegrees


scaleDegree : Scale -> Int -> Note
scaleDegree scale degree =
    let
        getIndexAt i list =
            List.head (List.drop i list)
    in
        case scale of
            Major ds ->
                let
                    degreeList =
                        [ ds.firstDegree
                        , ds.secondDegree
                        , ds.thirdDegree
                        , ds.fourthDegree
                        , ds.fifthDegree
                        , ds.sixthDegree
                        , ds.seventhDegree
                        ]
                in
                    Maybe.withDefault ds.firstDegree <|
                        getIndexAt
                            ((degree - 1) % (List.length degreeList))
                            degreeList

            Minor ds ->
                let
                    degreeList =
                        [ ds.firstDegree
                        , ds.secondDegree
                        , ds.thirdDegree
                        , ds.fourthDegree
                        , ds.fifthDegree
                        , ds.sixthDegree
                        , ds.seventhDegree
                        ]
                in
                    Maybe.withDefault ds.firstDegree <|
                        getIndexAt
                            ((degree - 1) % (List.length degreeList))
                            degreeList
