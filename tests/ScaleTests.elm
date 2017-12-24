module ScaleTests exposing (suite)

import Note exposing (Note(..), NoteName(..), Accidental(..))
import Scale exposing (Scale(..), notesInScale, ionian)
import Test exposing (..)
import Expect


suite : Test
suite =
    describe "Scale module"
        [ describe "Scale.notesInScale"
            [ test "returns notes of a scale specified" <|
                \_ ->
                    let
                        notes =
                            notesInScale (Scale (Note G Natural) ionian)
                    in
                        Expect.equal
                            notes
                            [ Note G Natural
                            , Note A Natural
                            , Note B Natural
                            , Note C Natural
                            , Note D Natural
                            , Note E Natural
                            , Note F Sharp
                            ]
            ]
        ]
