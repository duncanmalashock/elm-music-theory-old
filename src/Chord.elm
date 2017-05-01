module Chord exposing (..)

import Note exposing (Note(..))
import Scale exposing (..)
import Interval exposing (..)
import List.Extra


type alias Chord =
    List Note



-- majorTriadInScale : Scale -> Note -> Chord
-- majorTriadInScale scale tonic =
--
