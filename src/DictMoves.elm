module DictMoves exposing (..)

import Dict exposing (Dict)
import List.Extra exposing (findIndex)
import Utils exposing (insertAtIndex, removeItem)


type alias Parent a =
    { a | id : String, items : List String }


moveItem : Dict String (Parent a) -> { from : String, to : String } -> Dict String (Parent a)
moveItem dict { from, to } =
    case ( getStackByItem from dict, getStackByItem to dict ) of
        ( Just fromStack, Just toStack ) ->
            case findIndex (equals to) toStack.items of
                Just targetIndex ->
                    dict
                        |> updateStack fromStack.id (\s -> { s | items = removeItem from s.items })
                        |> updateStack toStack.id (\s -> { s | items = insertAtIndex targetIndex from s.items })

                Nothing ->
                    dict

        _ ->
            dict


moveItemToEndOfStack : Dict String (Parent a) -> { itemToMove : String, targetStack : String } -> Dict String (Parent a)
moveItemToEndOfStack dict { itemToMove, targetStack } =
    case ( getStackByItem itemToMove dict, Dict.get targetStack dict ) of
        ( Just fromStack, Just toStack ) ->
            dict
                |> updateStack fromStack.id (\s -> { s | items = removeItem itemToMove s.items })
                |> updateStack toStack.id (\s -> { s | items = List.append s.items [ itemToMove ] })

        _ ->
            dict


updateStack stackId updater dictS =
    Dict.update stackId (Maybe.map (\v -> updater v)) dictS


getStackByItem : String -> Dict String (Parent a) -> Maybe (Parent a)
getStackByItem item stacks =
    Dict.toList stacks
        |> List.filter (\( _, stack ) -> List.member item stack.items)
        |> List.map Tuple.second
        |> List.head


equals a b =
    a == b
