module Composition.Part
    exposing
        ( Part
        , calculate
        )

import Basics.Scale as Scale exposing (degreeOfScale)
import Basics.SpellIntervals as SpellIntervals exposing (getNoteAtIntervalFrom)
import Basics.Note as Note exposing (NoteName(..), Note(..), Accidental(..))


type alias Part =
    { steps : List Step
    , startingDegree :
        Int
    , scale : Scale.Scale
    , startingNote : Note.Note
    }


type alias Step =
    Int


calculate : Part -> List Note
calculate { steps, startingDegree, scale, startingNote } =
    List.map (degreeOfScale scale) (degreesFromSteps steps startingDegree [])
        |> List.map (getNoteAtIntervalFrom startingNote)


degreesFromSteps : List Step -> Int -> List Int -> List Int
degreesFromSteps steps startingDegree outputDegrees =
    case steps of
        [] ->
            List.reverse outputDegrees

        s :: tail ->
            let
                newDegree =
                    ((Maybe.withDefault 0 ((List.take 1 steps) |> List.head))
                        + startingDegree
                    )
            in
                degreesFromSteps
                    (List.drop 1 steps)
                    newDegree
                    (newDegree :: outputDegrees)
