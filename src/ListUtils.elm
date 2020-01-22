module ListUtils exposing (..)

import Dict


flipArguments f a b =
    f b a


removeItem itemToRemove list =
    List.filter ((/=) itemToRemove) list


ignoreNothing : List (Maybe item) -> List item
ignoreNothing maybes =
    List.filterMap identity maybes


getFlipped dict key =
    Dict.get key dict
