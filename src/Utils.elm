module Utils exposing (..)

import List.Extra exposing (findIndex, splitAt)


flip f a b =
    f b a


insertAtIndex : Int -> item -> List item -> List item
insertAtIndex index item ary =
    let
        ( left, right ) =
            ary
                |> splitAt index
                |> Tuple.mapSecond (\r -> item :: r)
    in
    [ left, right ]
        |> List.concat


removeItem : item -> List item -> List item
removeItem item items =
    List.filter (notEquals item) items


findLastItem : List item -> Maybe item
findLastItem list =
    List.drop (List.length list - 1) list |> List.head


equals : val -> val -> Bool
equals a b =
    a == b


notEquals : val -> val -> Bool
notEquals a b =
    a /= b


unpackMaybes : List (Maybe item) -> List item
unpackMaybes maybes =
    List.filterMap identity maybes
