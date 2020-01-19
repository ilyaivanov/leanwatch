module Utils exposing (..)


flipArguments f a b =
    f b a


ignoreNothing : List (Maybe item) -> List item
ignoreNothing maybes =
    List.filterMap identity maybes
