module ScaleTests exposing (suite)

import Note exposing (Note, LetterName(..), Accidental(..), note)
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
                            notesInScale (HeptatonicScale (note F Sharp) major)

                        expectedNotes =
                            [ note F Sharp
                            , note G Sharp
                            , note A Sharp
                            , note B Natural
                            , note C Sharp
                            , note D Sharp
                            , note E Sharp
                            ]

                        allTrue =
                            List.repeat (List.length expectedNotes) True
                    in
                        Expect.equal allTrue <| List.map (\n -> List.member n expectedNotes) notes
            ]
        ]
