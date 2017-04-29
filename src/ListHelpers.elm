module ListHelpers exposing (isSameList)


isSameList : List a -> List a -> Bool
isSameList listA listB =
    List.length listA
        == List.length listB
        && List.isEmpty (List.filter (not << flip List.member listB) listA)
