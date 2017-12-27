module ScaleTests exposing (suite)

import Note exposing (Note(..), LetterName(..), Accidental(..))
import Scale exposing (Scale(..), notesInScale, major)
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
                            notesInScale (HeptatonicScale (Note F Sharp) major)
                    in
                        Expect.equal
                            notes
                            [ Note F Sharp
                            , Note G Sharp
                            , Note A Sharp
                            , Note B Natural
                            , Note C Sharp
                            , Note D Sharp
                            , Note E Sharp
                            ]
            ]
        ]
