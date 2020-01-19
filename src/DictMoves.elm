module DictMoves exposing (..)

import Dict exposing (Dict)
import List.Extra exposing (findIndex, splitAt)


type alias Parent a =
    { a | id : String, children : List String }


moveItem : Dict String (Parent a) -> { from : String, to : String } -> Dict String (Parent a)
moveItem dict { from, to } =
    case ( getStackByItem from dict, getStackByItem to dict ) of
        ( Just fromStack, Just toStack ) ->
            case findIndex ((==) to) toStack.children of
                Just targetIndex ->
                    dict
                        |> updateStack fromStack.id (\s -> { s | children = removeItem from s.children })
                        |> updateStack toStack.id (\s -> { s | children = insertAtIndex targetIndex from s.children })

                Nothing ->
                    dict

        _ ->
            dict


moveItemToEnd : Dict String (Parent a) -> { itemToMove : String, targetParent : String } -> Dict String (Parent a)
moveItemToEnd dict { itemToMove, targetParent } =
    case ( getStackByItem itemToMove dict, Dict.get targetParent dict ) of
        ( Just fromStack, Just toStack ) ->
            dict
                |> updateStack fromStack.id (\s -> { s | children = removeItem itemToMove s.children })
                |> updateStack toStack.id (\s -> { s | children = List.append s.children [ itemToMove ] })

        _ ->
            dict


updateStack stackId updater =
    Dict.update stackId (Maybe.map updater)


getStackByItem : String -> Dict String (Parent a) -> Maybe (Parent a)
getStackByItem item stacks =
    Dict.toList stacks
        |> List.filter (\( _, stack ) -> List.member item stack.children)
        |> List.map Tuple.second
        |> List.head


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
    List.filter ((/=) item) items
