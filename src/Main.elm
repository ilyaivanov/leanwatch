module Main exposing (main)

import Browser
import Dict exposing (Dict)
import ExtraEvents exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (findIndex, splitAt)
import Process
import Random
import Task


main : Program (Maybe ()) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Lean Watch", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }


noComand : Model -> ( Model, Cmd Msg )
noComand model =
    ( model, Cmd.none )



-- MODEL


type alias Model =
    { stacks : Dict String Stack
    , items : Dict String Item
    , stacksOrder : List String
    , dragState : DragState
    , searchTerm : String
    , currentSearchId : String
    }


type DragState
    = NoDrag
    | DraggingItem MouseMoveEvent Offsets String
    | DraggingStack MouseMoveEvent Offsets String


type Msg
    = Noop
      --DND events
    | StackTitleMouseDown String MouseDownEvent
    | ItemMouseDown String MouseDownEvent
    | MouseMove MouseMoveEvent
    | MouseUp
    | StackOverlayEnterDuringDrag String
    | StackEnterDuringDrag String
    | ItemEnterDuringDrag String
      --Board
    | CreateStack String
    | OnSearchInput String
      --Commands
    | CreateSingleId
    | OnSearchDone String (List String)
      -- Debounced search
    | AttemptToSearch String
    | DebouncedSearch String String


init : Maybe () -> ( Model, Cmd Msg )
init _ =
    ( { stacks =
            Dict.fromList
                [ ( "1", Stack "1" "My Stack 1" (createItems 1 5) )
                , ( "2", Stack "2" "My Stack 42" (createItems 6 10) )
                , ( "42", Stack "42" "My Stack 2" (createItems 11 15) )
                , ( "Empty", Stack "Empty" "My Stack Empty" [] )
                , ( "SEARCH", Stack "SEARCH" "SEARCH_STACK" (createItems 16 20) )
                ]
      , items = Dict.fromList (List.range 1 20 |> List.map String.fromInt |> List.map (\id -> ( id, { id = id, name = "Item NEW " ++ id } )))
      , stacksOrder = [ "1", "42", "2", "Empty" ]
      , dragState = NoDrag
      , searchTerm = ""
      , currentSearchId = ""
      }
    , Cmd.none
    )


type alias Stack =
    { id : String
    , name : String
    , items : List String
    }


type alias Item =
    { id : String
    , name : String
    }


createItems : Int -> Int -> List String
createItems from to =
    List.map (\n -> String.fromInt n) (List.range from to)


isDraggingStack : DragState -> String -> Bool
isDraggingStack dragState stackId =
    case dragState of
        DraggingStack _ _ stackBeingDragged ->
            stackBeingDragged == stackId

        _ ->
            False


isDraggingItem : DragState -> Item -> Bool
isDraggingItem dragState { id } =
    case dragState of
        DraggingItem _ _ itemBeingDragged ->
            itemBeingDragged == id

        _ ->
            False


isDraggingAnyItem : DragState -> Bool
isDraggingAnyItem dragState =
    case dragState of
        DraggingItem _ _ _ ->
            True

        _ ->
            False


isDraggingAnything : DragState -> Bool
isDraggingAnything dragState =
    case dragState of
        NoDrag ->
            False

        _ ->
            True


getSearchStack : Model -> Maybe Stack
getSearchStack model =
    model.stacks |> Dict.get "SEARCH"


getStackToView model stackId =
    let
        stack =
            getStack stackId model.stacks

        items =
            stack.items |> List.map (\itemId -> Dict.get itemId model.items) |> unpackMaybes
    in
    ( stack, items )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemMouseDown itemId { mousePosition, offsets } ->
            noComand { model | dragState = DraggingItem mousePosition offsets itemId }

        MouseUp ->
            noComand { model | dragState = NoDrag }

        StackTitleMouseDown stackId { mousePosition, offsets } ->
            noComand { model | dragState = DraggingStack mousePosition offsets stackId }

        MouseMove newMousePosition ->
            case ( model.dragState, newMousePosition.buttons ) of
                ( DraggingStack _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingStack newMousePosition offsets id }

                ( DraggingItem _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingItem newMousePosition offsets id }

                -- Any registered mouse move without mouse pressed is ending eny drag session
                ( _, 0 ) ->
                    noComand { model | dragState = NoDrag }

                _ ->
                    noComand model

        ItemEnterDuringDrag itemUnder ->
            case model.dragState of
                DraggingItem _ _ itemOver ->
                    if itemOver == itemUnder then
                        noComand model

                    else
                        let
                            fromStack =
                                getStackByItem itemOver model.stacks

                            toStack =
                                getStackByItem itemUnder model.stacks

                            fromStackId =
                                fromStack.id

                            toStackId =
                                toStack.id

                            toStackItems =
                                toStack.items

                            targetIndex =
                                findIndex (equals itemUnder) toStackItems |> Maybe.withDefault -1

                            newStacks =
                                model.stacks
                                    |> updateStack fromStackId (\s -> { s | items = removeItem itemOver s.items })
                                    |> updateStack toStackId (\s -> { s | items = insertInto targetIndex itemOver s.items })
                        in
                        noComand { model | stacks = newStacks }

                _ ->
                    noComand model

        StackEnterDuringDrag stackUnder ->
            case model.dragState of
                DraggingStack _ _ stackOver ->
                    noComand { model | stacksOrder = moveStackToAnotherPosition model stackOver stackUnder }

                _ ->
                    noComand model

        StackOverlayEnterDuringDrag stackUnder ->
            case model.dragState of
                DraggingStack _ _ stackOver ->
                    noComand { model | stacksOrder = moveStackToAnotherPosition model stackOver stackUnder }

                DraggingItem _ _ itemOver ->
                    let
                        fromStack =
                            getStackByItem itemOver model.stacks

                        toStack =
                            getStack stackUnder model.stacks

                        fromStackId =
                            fromStack.id

                        toStackId =
                            toStack.id

                        toItems =
                            toStack.items
                    in
                    case findLastItem toItems of
                        Just lastItem ->
                            if lastItem == itemOver then
                                noComand model

                            else
                                noComand
                                    { model
                                        | stacks =
                                            model.stacks
                                                |> updateStack fromStackId (\s -> { s | items = removeItem itemOver s.items })
                                                |> updateStack toStackId (\s -> { s | items = List.append s.items [ itemOver ] })
                                    }

                        Nothing ->
                            noComand
                                { model
                                    | stacks =
                                        model.stacks
                                            |> updateStack fromStackId (\s -> { s | items = removeItem itemOver s.items })
                                            |> updateStack toStackId (\s -> { s | items = [ itemOver ] })
                                }

                _ ->
                    noComand model

        CreateStack newStackId ->
            noComand
                { model
                    | stacksOrder = List.append model.stacksOrder [ newStackId ]
                    , stacks = Dict.insert newStackId (Stack newStackId "New Stack" []) model.stacks
                }

        OnSearchInput val ->
            ( { model | searchTerm = val }
            , Random.generate AttemptToSearch createId
            )

        CreateSingleId ->
            ( model, Random.generate CreateStack createId )

        AttemptToSearch searchId ->
            ( { model | currentSearchId = searchId }, Process.sleep 500 |> Task.perform (always (DebouncedSearch searchId model.searchTerm)) )

        DebouncedSearch id term ->
            if id == model.currentSearchId then
                ( { model | currentSearchId = "" }, Random.generate (OnSearchDone term) createIds )

            else
                ( model, Cmd.none )

        OnSearchDone postfix ids ->
            let
                newItems =
                    ids |> List.map (\id -> Item id ("ITEM " ++ postfix ++ String.slice 2 3 id))

                newItemsDict =
                    Dict.fromList (List.map (\i -> ( i.id, i )) newItems)

                itemsUpdated =
                    Dict.union newItemsDict model.items
            in
            noComand { model | stacks = Dict.update "SEARCH" (\_ -> Just (Stack "SEARCH" "New Stack" ids)) model.stacks, items = itemsUpdated }

        Noop ->
            noComand model


createIds =
    Random.list 10 createId


createId =
    Random.float 0 1 |> Random.map String.fromFloat


unpackMaybes : List (Maybe item) -> List item
unpackMaybes maybes =
    List.filterMap identity maybes


moveStackToAnotherPosition model stackOver stackUnder =
    let
        targetIndex =
            findIndex (equals stackUnder) model.stacksOrder |> Maybe.withDefault -1
    in
    model.stacksOrder
        |> removeItem stackOver
        |> insertInto targetIndex stackOver


updateStack : String -> (Stack -> Stack) -> Dict String Stack -> Dict String Stack
updateStack stackId updater stacks =
    Dict.update stackId (Maybe.map (\v -> updater v)) stacks


getStackByItem : String -> Dict String Stack -> Stack
getStackByItem item stacks =
    Dict.toList stacks
        |> List.filter (\( _, stack ) -> List.member item stack.items)
        |> List.map Tuple.second
        |> List.head
        |> Maybe.withDefault (Stack "NOT_FOUND" "NOT_FOUND" [])


getStack : String -> Dict String Stack -> Stack
getStack stackId stacks =
    Dict.get stackId stacks |> Maybe.withDefault (Stack "NOT_FOUND" "NOT_FOUND" [])


insertInto : Int -> item -> List item -> List item
insertInto index item ary =
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



-- VIEW


view : Model -> Html Msg
view model =
    div (attributeIf (isDraggingAnything model.dragState) (onMouseMove MouseMove))
        [ viewSidebar model
        , div [ class "page-content" ]
            [ div
                [ class "board", classIf (isDraggingAnything model.dragState) "board-during-drag", onMouseUp MouseUp ]
                (List.append
                    (model.stacksOrder
                        |> List.map (\stackId -> Dict.get stackId model.stacks)
                        |> unpackMaybes
                        |> List.map (\stack -> viewStack model.dragState [ class "column-board" ] (getStackToView model stack.id))
                    )
                    [ button [ class "add-stack-button", onClick CreateSingleId ] [ text "add" ], viewElementBeingDragged model ]
                )
            ]
        ]


viewStack : DragState -> List (Attribute Msg) -> ( Stack, List Item ) -> Html Msg
viewStack dragState attributes ( { id, name }, items ) =
    div (List.append [ class "column-drag-overlay" ] attributes)
        [ div
            [ class "column"
            , classIf (isDraggingStack dragState id) "column-preview"
            , onMouseEnter (StackEnterDuringDrag id)
            ]
            [ div [ class "column-title", onMouseDown (StackTitleMouseDown id) ]
                [ span [] [ text name ]
                ]
            , div [ class "column-content" ]
                (if List.isEmpty items then
                    [ div [ class "empty-stack-placeholder", onMouseEnter (StackOverlayEnterDuringDrag id) ] [] ]

                 else
                    List.map (\item -> viewItem (isDraggingItem dragState item) item) items
                )
            ]
        , div [ class "column-footer", onMouseEnter (StackOverlayEnterDuringDrag id) ] []
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        stackM =
            getSearchStack model

        items =
            case stackM of
                Just stack ->
                    stack.items |> List.map (\itemId -> Dict.get itemId model.items) |> unpackMaybes

                Nothing ->
                    []
    in
    div [ class "sidebar" ]
        [ input [ onInput OnSearchInput, placeholder "Find videos by name...", value model.searchTerm ] []
        , div [] (List.map (\item -> viewItem (isDraggingItem model.dragState item) item) items)
        ]


viewItem : Bool -> Item -> Html Msg
viewItem isDragging { id, name } =
    div
        [ class "item"
        , classIf isDragging "item-preview"
        , onMouseDown (ItemMouseDown id)
        , onMouseEnter (ItemEnterDuringDrag id)
        ]
        [ text name ]


viewElementBeingDragged model =
    case model.dragState of
        DraggingItem mouseMoveEvent offsets itemId ->
            div
                [ class "item item-dragged"
                , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                ]
                [ text (model.items |> Dict.get itemId |> Maybe.withDefault (Item "1" "1")).name ]

        DraggingStack mouseMoveEvent offsets stackId ->
            let
                ( stack, items ) =
                    getStackToView model stackId
            in
            viewStack NoDrag
                [ class "item-dragged"
                , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                ]
                ( stack, items )

        _ ->
            div [] []



-- CSS HELPERS


classIf : Bool -> String -> Attribute msg
classIf condition className =
    if condition then
        class className

    else
        class ""


attributeIf : Bool -> Attribute msg -> List (Attribute msg)
attributeIf condition attribute =
    if condition then
        [ attribute ]

    else
        []
