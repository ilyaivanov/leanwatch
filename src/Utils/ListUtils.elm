module Utils.ListUtils exposing (..)

import List.Extra as LE exposing (..)


flip f a b =
    f b a


removeItem itemToRemove list =
    List.filter ((/=) itemToRemove) list


unpackMaybes : List (Maybe item) -> List item
unpackMaybes maybes =
    List.filterMap identity maybes


getNextItem : a -> List a -> Maybe a
getNextItem item list =
    list
        |> LE.elemIndex item
        |> Maybe.map ((+) 1)
        |> Maybe.andThen (\i -> LE.getAt i list)
