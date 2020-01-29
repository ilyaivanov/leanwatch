module Board exposing (..)

import Dict exposing (Dict)
import DictMoves exposing (Parent, getParentByChildren)
import Json.Decode as Json
import Utils.Other exposing (getNextItem, removeItem, unpackMaybes)


type alias Board =
    Parent
        { name : String
        }


type alias Stack =
    Parent
        { name : String
        , stackState : StackStatus
        }


type StackStatus
    = Ready (Maybe String)
    | IsLoading
    | IsLoadingNextPage


type alias Item =
    { id : String
    , name : String
    , itemId : String
    , itemType : String
    , image : Maybe String
    , duration : Maybe Float
    , currentTime : Maybe Float
    }


type alias BoardModel =
    { boards : Dict String Board
    , stacks : Dict String Stack
    , items : Dict String Item
    }


makeStack id name =
    { id = id, name = name, children = [], stackState = Ready Nothing }


init : BoardModel
init =
    { stacks =
        Dict.fromList
            [ ( "SEARCH", makeStack "SEARCH" "SEARCH" )
            , ( "SIMILAR", makeStack "SIMILAR" "SIMILAR" )
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


getStackToView : BoardModel -> String -> Maybe ( Stack, List Item )
getStackToView model stackId =
    getStack stackId model
        |> Maybe.map
            (\actualStack ->
                ( actualStack, actualStack.children |> List.map (\itemId -> Dict.get itemId model.items) |> unpackMaybes )
            )


getItemById : BoardModel -> String -> Maybe Item
getItemById model itemId =
    Dict.toList model.items
        |> List.map Tuple.second
        |> List.filter (\i -> i.id == itemId)
        |> List.head


getStack : String -> BoardModel -> Maybe Stack
getStack stackId model =
    Dict.get stackId model.stacks


getStackStatus : String -> BoardModel -> Maybe StackStatus
getStackStatus stackId model =
    getStack stackId model |> Maybe.map .stackState


setStacks stacks model =
    { model | stacks = stacks }


setBoards boards model =
    { model | boards = boards }


createStack boardId newStackId model =
    let
        newStack =
            { id = newStackId, name = "New Stack", children = [], stackState = Ready Nothing }

        newStacks =
            Dict.insert newStackId newStack model.stacks
    in
    model
        |> setStacks newStacks
        |> updateBoard boardId (\b -> { b | children = List.append b.children [ newStackId ] })


removeStack boardId stackId model =
    model
        |> updateBoard boardId (\b -> { b | children = removeItem stackId b.children })


getIds =
    List.map .id


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


updateItem itemId updater model =
    { model | items = Dict.update itemId (Maybe.map (\v -> updater v)) model.items }


updateStack stackId updater model =
    { model | stacks = Dict.update stackId (Maybe.map (\v -> updater v)) model.stacks }


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


updateTimeline videoPlayed timeline model =
    updateItem videoPlayed (\item -> { item | duration = Just timeline.duration, currentTime = Just timeline.currentTime }) model


updateName itemId newName model =
    { model
        | boards = updateNameInDict model.boards itemId newName
        , stacks = updateNameInDict model.stacks itemId newName
    }


updateNameInDict : Dict String { a | name : String } -> String -> String -> Dict String { a | name : String }
updateNameInDict boards boardId newName =
    Dict.update boardId (Maybe.map (\s -> { s | name = newName })) boards


startLoading stackId model =
    model |> updateStack stackId (\s -> { s | stackState = IsLoading })


startLoadingNextPage stackId model =
    model |> updateStack stackId (\s -> { s | stackState = IsLoadingNextPage })


onStackLoadingDone : String -> String -> BoardModel -> BoardModel
onStackLoadingDone stackId nextPageToken model =
    model |> updateStack stackId (\s -> { s | stackState = Ready (Just nextPageToken) })



--API HANDLING


type alias BoardResponse =
    { id : String
    , name : String
    , stacks : List StackResponse
    }


type alias StackResponse =
    { id : String
    , name : String
    , items : List Item
    }


type alias TimeLineInfo =
    { duration : Float
    , currentTime : Float
    }


decodeResponse : Json.Decoder (List BoardResponse)
decodeResponse =
    Json.list decodeBoard


decodeBoard : Json.Decoder BoardResponse
decodeBoard =
    Json.map3 BoardResponse
        (Json.field "id" Json.string)
        (Json.field "name" Json.string)
        (Json.field "stacks" (Json.list decodeStack))


decodeStack : Json.Decoder StackResponse
decodeStack =
    Json.map3 StackResponse
        (Json.field "id" Json.string)
        (Json.field "name" Json.string)
        (Json.field "items" (Json.list decodeItem))


decodeItem : Json.Decoder Item
decodeItem =
    Json.map7 Item
        (Json.field "id" Json.string)
        (Json.field "name" Json.string)
        (Json.oneOf [ Json.field "youtubeId" Json.string, Json.field "itemId" Json.string ])
        (Json.oneOf [ Json.field "itemType" Json.string, Json.field "type" Json.string, Json.succeed "video" ])
        (Json.maybe (Json.field "image" Json.string))
        (Json.maybe (Json.field "duration" Json.float))
        (Json.maybe (Json.field "currentTime" Json.float))


mergeAndNormalizeResponse : BoardResponse -> BoardModel -> BoardModel
mergeAndNormalizeResponse boardResponse model =
    let
        stacks =
            Dict.fromList (List.map (\s -> ( s.id, { id = s.id, name = s.name, children = getIds s.items, stackState = Ready Nothing } )) boardResponse.stacks)

        allItems =
            boardResponse.stacks |> List.map (\s -> s.items) |> List.concat

        items =
            Dict.fromList (List.map (\i -> ( i.id, i )) allItems)
    in
    { boards = Dict.insert boardResponse.id { id = boardResponse.id, name = boardResponse.name, children = getIds boardResponse.stacks } model.boards
    , stacks = Dict.union stacks model.stacks
    , items = Dict.union items model.items
    }


denormalizeBoard : String -> BoardModel -> Maybe BoardResponse
denormalizeBoard boardId model =
    Dict.get boardId model.boards
        |> Maybe.map
            (\board ->
                let
                    stacksNarrow =
                        board.children |> List.map (\id -> Dict.get id model.stacks) |> unpackMaybes

                    stacks =
                        stacksNarrow |> List.map (\s -> { id = s.id, name = s.name, items = s.children |> List.map (\id -> Dict.get id model.items) |> unpackMaybes })
                in
                { id = board.id
                , name = board.name
                , stacks = stacks
                }
            )
