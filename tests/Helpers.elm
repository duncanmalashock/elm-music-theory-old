module Helpers exposing (listsEqual)

import Set
import Expect


listsEqual : List a -> List a -> Expect.Expectation
listsEqual listA listB =
    Expect.equalSets
        (Set.fromList <| List.map toString listA)
        (Set.fromList <| List.map toString listB)
