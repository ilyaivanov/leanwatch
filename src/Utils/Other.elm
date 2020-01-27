module Utils.Other exposing (..)

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


ifNothing maybeToCheck otherMaybe =
    case maybeToCheck of
        Just a ->
            Just a

        Nothing ->
            otherMaybe


maybeHasValue maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False
