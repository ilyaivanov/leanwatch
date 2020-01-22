module ListUtils exposing (..)


flip f a b =
    f b a


removeItem itemToRemove list =
    List.filter ((/=) itemToRemove) list


unpackMaybes : List (Maybe item) -> List item
unpackMaybes maybes =
    List.filterMap identity maybes
