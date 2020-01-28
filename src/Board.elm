module Board exposing (..)

import Dict exposing (Dict)
import DictMoves exposing (Parent, getParentByChildren)
import Utils.Other exposing (getNextItem, removeItem, unpackMaybes)


type alias Board =
    Parent
        { name : String
        }


type alias Stack =
    Parent
        { name : String
        }


type alias Item =
    { id : String
    , name : String
    , youtubeId : String
    }


type alias BoardModel =
    { boards : Dict String Board
    , stacks : Dict String Stack
    , items : Dict String Item
    }


init : BoardModel
init =
    { stacks =
        Dict.fromList
            [ ( "SEARCH", { id = "SEARCH", name = "SEARCH", children = [] } )
            , ( "SIMILAR", { id = "SIMILAR", name = "SIMILAR", children = [] } )
            ]
    , items = Dict.empty
    , boards = Dict.empty
    }


getItemsForStack : String -> BoardModel -> List Item
getItemsForStack stackName model =
    model.stacks
        |> Dict.get stackName
        |> Maybe.map .children
        |> Maybe.withDefault []
        |> List.map (\itemId -> Dict.get itemId model.items)
        |> unpackMaybes


getStackToView : BoardModel -> String -> ( Stack, List Item )
getStackToView model stackId =
    let
        stack =
            getStack stackId model.stacks

        items =
            stack.children |> List.map (\itemId -> Dict.get itemId model.items) |> unpackMaybes
    in
    ( stack, items )


getItemById : BoardModel -> String -> Maybe Item
getItemById model itemId =
    Dict.toList model.items
        |> List.map Tuple.second
        |> List.filter (\i -> i.id == itemId)
        |> List.head


getStack : String -> Dict String Stack -> Stack
getStack stackId stacks =
    Dict.get stackId stacks |> Maybe.withDefault { id = "NOT_FOUND", name = "NOT_FOUND", children = [] }


setStacks stacks model =
    { model | stacks = stacks }


setBoards boards model =
    { model | boards = boards }


createStack boardId newStackId model =
    let
        newStack =
            { id = newStackId, name = "New Stack", children = [] }

        newStacks =
            Dict.insert newStackId newStack model.stacks
    in
    model
        |> setStacks newStacks
        |> updateBoard boardId (\b -> { b | children = List.append b.children [ newStackId ] })


removeStack boardId stackId model =
    model
        |> updateBoard boardId (\b -> { b | children = removeItem stackId b.children })


getIds items =
    List.map .id items


toDic items =
    Dict.fromList (List.map (\i -> ( i.id, i )) items)


setStackChildren stackId items model =
    { model
        | stacks = Dict.update stackId (Maybe.map (\s -> { s | children = getIds items })) model.stacks
        , items = Dict.union (toDic items) model.items
    }


appendStackChildren stackId items model =
    { model
        | stacks = Dict.update stackId (Maybe.map (\s -> { s | children = List.append s.children (getIds items) })) model.stacks
        , items = Dict.union (toDic items) model.items
    }


removeBoard boardId model =
    { model | boards = Dict.remove boardId model.boards }


updateBoard boardId updater model =
    { model | boards = Dict.update boardId (Maybe.map (\v -> updater v)) model.boards }


getNextItemInStack : Maybe String -> BoardModel -> Maybe Item
getNextItemInStack itemIdM model =
    let
        stack =
            getParentByChildren (itemIdM |> Maybe.withDefault "") model.stacks
    in
    case ( stack, itemIdM ) of
        ( Just actualStack, Just itemId ) ->
            case getNextItem itemId actualStack.children of
                Just nextItemId ->
                    case getItemById model nextItemId of
                        Just nextItem ->
                            Just nextItem

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        _ ->
            Nothing


updateName itemId newName model =
    { model
        | boards = updateNameInDict model.boards itemId newName
        , stacks = updateNameInDict model.stacks itemId newName
    }


updateNameInDict : Dict String { a | name : String } -> String -> String -> Dict String { a | name : String }
updateNameInDict boards boardId newName =
    Dict.update boardId (Maybe.map (\s -> { s | name = newName })) boards
