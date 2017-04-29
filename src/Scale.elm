module Scale exposing (Scale(..))

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
