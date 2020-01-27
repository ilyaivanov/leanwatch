module DictMoves exposing (..)

import Dict exposing (Dict)
import List.Extra exposing (findIndex, splitAt)
import Utils.Other exposing (removeItem)


type alias Parent a =
    { a | id : String, children : List String }


moveItem : Dict String (Parent a) -> { from : String, to : String } -> Dict String (Parent a)
moveItem dict { from, to } =
    case ( getParentByChildren from dict, getParentByChildren to dict ) of
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


moveItemInList : List String -> { from : String, to : String } -> List String
moveItemInList items { from, to } =
    case findIndex ((==) to) items of
        Just index ->
            items |> removeItem from |> insertAtIndex index from

        Nothing ->
            items


moveItemToEnd : Dict String (Parent a) -> { itemToMove : String, targetParent : String } -> Dict String (Parent a)
moveItemToEnd dict { itemToMove, targetParent } =
    case ( getParentByChildren itemToMove dict, Dict.get targetParent dict ) of
        ( Just fromStack, Just toStack ) ->
            dict
                |> updateStack fromStack.id (\s -> { s | children = removeItem itemToMove s.children })
                |> updateStack toStack.id (\s -> { s | children = List.append s.children [ itemToMove ] })

        _ ->
            dict


updateStack stackId updater =
    Dict.update stackId (Maybe.map updater)


getParentByChildren : String -> Dict String (Parent a) -> Maybe (Parent a)
getParentByChildren item stacks =
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
