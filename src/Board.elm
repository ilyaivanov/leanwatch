port module Board exposing (Item, Model, Msg(..), Stack, createBoard, init, onBoardCreated, onBoardsLoaded, onUserProfileLoaded, update, view)

import Browser.Dom as Dom exposing (focus)
import Dict exposing (Dict)
import DictMoves exposing (Parent, getParentByChildren, moveItem, moveItemInList, moveItemToEnd)
import Embed.Youtube
import Embed.Youtube.Attributes
import ExtraEvents exposing (..)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Json.Decode as Json
import ListUtils exposing (flipArguments, ignoreNothing, removeItem)
import Login
import Process
import Random
import Set exposing (Set)
import Task


noComand : Model -> ( Model, Cmd msg )
noComand model =
    ( model, Cmd.none )


port onUserProfileLoaded : (UserProfile -> msg) -> Sub msg


port onBoardsLoaded : (List BoardResponse -> msg) -> Sub msg


port saveBoard : List BoardResponse -> Cmd msg


port saveProfile : UserProfile -> Cmd msg


port createBoard : () -> Cmd msg


port onBoardCreated : (BoardResponse -> msg) -> Sub msg



-- NORMALIZATION to extract


type alias BoardResponse =
    { id : String
    , name : String
    , stacks :
        List
            { id : String
            , name : String
            , items :
                List
                    { id : String
                    , name : String
                    , youtubeId : String
                    }
            }
    }


mergeAndNormalizeResponse : BoardResponse -> Model -> Model
mergeAndNormalizeResponse boardResponse model =
    let
        stacks =
            Dict.fromList (List.map (\s -> ( s.id, { id = s.id, name = s.name, children = getIds s.items } )) boardResponse.stacks)

        allItems =
            boardResponse.stacks |> List.map (\s -> s.items) |> List.concat

        items =
            Dict.fromList (List.map (\i -> ( i.id, { id = i.id, name = i.name, youtubeId = i.youtubeId } )) allItems)

        getIds =
            List.map (\s -> s.id)
    in
    { model
        | boards = Dict.insert boardResponse.id { id = boardResponse.id, name = boardResponse.name, children = getIds boardResponse.stacks } model.boards
        , stacks = Dict.union stacks model.stacks
        , items = Dict.union items model.items
    }


denormalizeBoard : String -> Model -> Maybe BoardResponse
denormalizeBoard boardId model =
    Dict.get boardId model.boards
        |> Maybe.map
            (\board ->
                let
                    stacksNarrow =
                        board.children |> List.map (\id -> Dict.get id model.stacks) |> ignoreNothing

                    stacks =
                        stacksNarrow |> List.map (\s -> { id = s.id, name = s.name, items = s.children |> List.map (\id -> Dict.get id model.items) |> ignoreNothing })
                in
                { id = board.id
                , name = board.name
                , stacks = stacks
                }
            )



-- MODEL


type alias Model =
    { boards : Dict String Board
    , stacks : Dict String Stack
    , items : Dict String Item
    , videoBeingPlayed : Maybe String
    , userProfile : UserProfile

    -- UI STATE
    , sidebarState : SidebarState
    , dragState : DragState
    , renamingState : RenamingState
    , boardIdsToSync : Set String
    , needToSyncProfile : Bool
    , searchTerm : String

    -- Used to debounce search on input
    , currentSearchId : String
    }


type DragState
    = NoDrag
    | ItemPressedNotYetMoved MouseMoveEvent Offsets String
    | BoardPressedNotYetMoved MouseMoveEvent Offsets String
    | DraggingItem MouseMoveEvent Offsets String
    | DraggingStack MouseMoveEvent Offsets String
    | DraggingBoard MouseMoveEvent Offsets String


type RenamingState
    = NoRename
    | RenamingItem { itemId : String, newName : String }


type SidebarState
    = Hidden
    | Search
    | Boards


type Msg
    = Noop
      -- DND events
    | StackTitleMouseDown String MouseDownEvent
    | ItemMouseDown String MouseDownEvent
    | BoardMouseDown String MouseDownEvent
    | MouseMove MouseMoveEvent
    | MouseUp
    | StackOverlayEnterDuringDrag String
    | StackEnterDuringDrag String
    | BoardEnterDuringDrag String
    | ItemEnterDuringDrag String
      -- Board
    | CreateStack String
    | OnSearchInput String
      -- Commands
    | CreateSingleId
      -- Debounced search
    | AttemptToSearch String
    | DebouncedSearch String String
    | GotItems (Result Http.Error SearchResponse)
      -- Sidebar
    | SetSidebar SidebarState
      -- Board Management
    | SelectBoard String
    | UserProfileLoaded UserProfile
    | BoardsLoaded (List BoardResponse)
    | OnBoardCreated BoardResponse
    | CreateNewBoard
    | RemoveBoard String
    | RemoveStack String
    | StartModifyingItem { itemId : String, newName : String }
    | OnNewNameEnter String
    | ApplyModification
    | OnModificationKeyUp String
    | FocusResult (Result Dom.Error ())
      -- Backend sync
    | SaveModifiedItemsOnDemand
    | SaveModifiedItemsScheduled


init : Model
init =
    { stacks = Dict.empty
    , items = Dict.empty
    , boards = Dict.empty
    , userProfile =
        { selectedBoard = ""
        , boards = []
        , id = ""
        }
    , boardIdsToSync = Set.empty
    , needToSyncProfile = False
    , dragState = NoDrag
    , renamingState = NoRename
    , searchTerm = ""
    , currentSearchId = ""
    , videoBeingPlayed = Nothing
    , sidebarState = Boards
    }


type alias Board =
    Parent
        { name : String
        }


type alias Stack =
    Parent
        { name : String
        }


type alias UserProfile =
    { id : String
    , boards : List String
    , selectedBoard : String
    }


type alias Item =
    { id : String
    , name : String
    , youtubeId : String
    }


type alias SearchResponse =
    { items : List Item
    }


isDraggingStack : DragState -> String -> Bool
isDraggingStack dragState stackId =
    case dragState of
        DraggingStack _ _ stackBeingDragged ->
            stackBeingDragged == stackId

        _ ->
            False


isDraggingBoard : DragState -> String -> Bool
isDraggingBoard dragState boardId =
    case dragState of
        DraggingBoard _ _ boardBeingDragged ->
            boardBeingDragged == boardId

        _ ->
            False


isDraggingAnyBoard : DragState -> Bool
isDraggingAnyBoard dragState =
    case dragState of
        DraggingBoard _ _ _ ->
            True

        _ ->
            False


isDraggingItem : DragState -> Item -> Bool
isDraggingItem dragState { id } =
    case dragState of
        DraggingItem _ _ itemBeingDragged ->
            itemBeingDragged == id

        _ ->
            False


isDraggingAnything : DragState -> Bool
isDraggingAnything dragState =
    case dragState of
        NoDrag ->
            False

        ItemPressedNotYetMoved _ _ _ ->
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
            stack.children |> List.map (\itemId -> Dict.get itemId model.items) |> ignoreNothing
    in
    ( stack, items )


getItemById : String -> Model -> Maybe Item
getItemById itemId model =
    Dict.toList model.items
        |> List.map Tuple.second
        |> List.filter (\i -> i.id == itemId)
        |> List.head


getStack : String -> Dict String Stack -> Stack
getStack stackId stacks =
    Dict.get stackId stacks |> Maybe.withDefault { id = "NOT_FOUND", name = "NOT_FOUND", children = [] }


getBoardViewModel : Model -> Maybe Board
getBoardViewModel model =
    Dict.get model.userProfile.selectedBoard model.boards



-- UPDATE


markSelectedBoardAsNeededToSync model =
    { model | boardIdsToSync = Set.insert model.userProfile.selectedBoard model.boardIdsToSync }


updateProfileAndMarkAsNeededToSync profile model =
    { model | userProfile = profile, needToSyncProfile = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemMouseDown itemId { mousePosition, offsets } ->
            noComand { model | dragState = ItemPressedNotYetMoved mousePosition offsets itemId }

        BoardMouseDown itemId { mousePosition, offsets } ->
            noComand { model | dragState = BoardPressedNotYetMoved mousePosition offsets itemId }

        MouseUp ->
            case model.dragState of
                ItemPressedNotYetMoved _ _ id ->
                    noComand { model | dragState = NoDrag, videoBeingPlayed = Just id }

                _ ->
                    noComand { model | dragState = NoDrag }

        StackTitleMouseDown stackId { mousePosition, offsets } ->
            case model.renamingState of
                --Ignore drag clicks during modification
                RenamingItem _ ->
                    noComand model

                _ ->
                    noComand { model | dragState = DraggingStack mousePosition offsets stackId }

        MouseMove newMousePosition ->
            case ( model.dragState, newMousePosition.buttons ) of
                ( DraggingStack _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingStack newMousePosition offsets id }

                ( ItemPressedNotYetMoved _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingItem newMousePosition offsets id }

                ( DraggingItem _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingItem newMousePosition offsets id }

                ( BoardPressedNotYetMoved _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingBoard newMousePosition offsets id }

                ( DraggingBoard _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingBoard newMousePosition offsets id }

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
                        { model | stacks = moveItem model.stacks { from = itemOver, to = itemUnder } } |> markSelectedBoardAsNeededToSync |> noComand

                _ ->
                    noComand model

        StackEnterDuringDrag stackUnder ->
            case model.dragState of
                DraggingStack _ _ stackOver ->
                    { model | boards = moveItem model.boards { from = stackOver, to = stackUnder } } |> markSelectedBoardAsNeededToSync |> noComand

                _ ->
                    noComand model

        BoardEnterDuringDrag boardUnder ->
            case model.dragState of
                DraggingBoard _ _ boardOver ->
                    let
                        profile =
                            model.userProfile

                        newOrder =
                            moveItemInList model.userProfile.boards { from = boardOver, to = boardUnder }
                    in
                    noComand (updateProfileAndMarkAsNeededToSync { profile | boards = newOrder } model)

                _ ->
                    noComand model

        StackOverlayEnterDuringDrag stackUnder ->
            case model.dragState of
                DraggingStack _ _ stackOver ->
                    { model | boards = moveItem model.boards { from = stackOver, to = stackUnder } } |> markSelectedBoardAsNeededToSync |> noComand

                DraggingItem _ _ itemOver ->
                    { model | stacks = moveItemToEnd model.stacks { itemToMove = itemOver, targetParent = stackUnder } } |> markSelectedBoardAsNeededToSync |> noComand

                _ ->
                    noComand model

        CreateStack newStackId ->
            { model
                | boards = updateBoard model.userProfile.selectedBoard (\b -> { b | children = List.append b.children [ newStackId ] }) model.boards
                , stacks = Dict.insert newStackId { id = newStackId, name = "New Stack", children = [] } model.stacks
            }
                |> markSelectedBoardAsNeededToSync
                |> noComand

        OnSearchInput val ->
            ( { model | searchTerm = val }
            , Random.generate AttemptToSearch createId
            )

        CreateSingleId ->
            ( model, Random.generate CreateStack createId )

        AttemptToSearch searchId ->
            ( { model | currentSearchId = searchId }, Process.sleep 500 |> Task.perform (always (DebouncedSearch searchId model.searchTerm)) )

        DebouncedSearch id term ->
            let
                request =
                    Http.get
                        { url = "https://us-central1-lean-watch.cloudfunctions.net/getVideos?q=" ++ term
                        , expect = Http.expectJson GotItems decodeItems
                        }
            in
            if id == model.currentSearchId then
                ( { model | currentSearchId = "" }, request )

            else
                ( model, Cmd.none )

        SaveModifiedItemsOnDemand ->
            saveModifiedItems model

        SaveModifiedItemsScheduled ->
            let
                ( newModel, cmds ) =
                    saveModifiedItems model
            in
            ( newModel, Cmd.batch [ cmds, scheduleNextSync ] )

        GotItems response ->
            case response of
                Ok body ->
                    let
                        ids =
                            List.map .id body.items

                        newItemsDict =
                            Dict.fromList (List.map (\i -> ( i.id, i )) body.items)

                        itemsUpdated =
                            Dict.union newItemsDict model.items
                    in
                    noComand { model | stacks = Dict.update "SEARCH" (\_ -> Just { id = "SEARCH", name = "New Stack", children = ids }) model.stacks, items = itemsUpdated }

                _ ->
                    noComand model

        SetSidebar state ->
            noComand { model | sidebarState = state }

        SelectBoard boardId ->
            if boardId == model.userProfile.selectedBoard then
                ( model, Cmd.none )

            else
                let
                    profile =
                        model.userProfile
                in
                noComand (updateProfileAndMarkAsNeededToSync { profile | selectedBoard = boardId } model)

        UserProfileLoaded profile ->
            ( { model | userProfile = profile }, scheduleNextSync )

        BoardsLoaded boards ->
            noComand (List.foldl mergeAndNormalizeResponse model boards)

        CreateNewBoard ->
            ( model, createBoard () )

        OnBoardCreated boardResponse ->
            let
                profile =
                    model.userProfile

                newProfile =
                    { profile | selectedBoard = boardResponse.id, boards = profile.boards ++ [ boardResponse.id ] }
            in
            ( mergeAndNormalizeResponse boardResponse model |> updateProfileAndMarkAsNeededToSync newProfile, Cmd.none )

        StartModifyingItem item ->
            ( { model | renamingState = RenamingItem item }, focus item.itemId |> Task.attempt FocusResult )

        OnNewNameEnter newName ->
            case model.renamingState of
                RenamingItem mod ->
                    ( { model | renamingState = RenamingItem { mod | newName = newName } }, Cmd.none )

                NoRename ->
                    ( model, Cmd.none )

        ApplyModification ->
            ( finishModification model, Cmd.none )

        OnModificationKeyUp key ->
            if key == "Enter" || key == "Escape" then
                ( finishModification model, Cmd.none )

            else
                ( model, Cmd.none )

        RemoveBoard boardId ->
            let
                profile =
                    model.userProfile

                newBoards =
                    removeItem boardId profile.boards

                nextSelectedBoard =
                    if profile.selectedBoard == boardId then
                        List.head newBoards |> Maybe.withDefault ""

                    else
                        profile.selectedBoard

                newProfile =
                    { profile | boards = removeItem boardId profile.boards, selectedBoard = nextSelectedBoard }
            in
            noComand ({ model | boards = Dict.remove boardId model.boards } |> updateProfileAndMarkAsNeededToSync newProfile)

        RemoveStack stackId ->
            case getParentByChildren stackId model.boards of
                Just board ->
                    let
                        newBoard =
                            { board | children = removeItem stackId board.children }
                    in
                    { model | boards = Dict.insert board.id newBoard model.boards } |> markSelectedBoardAsNeededToSync |> noComand

                Nothing ->
                    noComand model

        Noop ->
            noComand model

        FocusResult _ ->
            noComand model


oneMinute =
    1000 * 60


scheduleNextSync =
    Process.sleep oneMinute |> Task.perform (always SaveModifiedItemsScheduled)


saveModifiedItems : Model -> ( Model, Cmd Msg )
saveModifiedItems model =
    let
        syncProfile =
            if model.needToSyncProfile then
                saveProfile model.userProfile

            else
                Cmd.none

        syncBoards =
            Set.toList model.boardIdsToSync |> List.map (\boardId -> denormalizeBoard boardId model) |> ignoreNothing |> saveBoard
    in
    ( { model | needToSyncProfile = False, boardIdsToSync = Set.empty }, Cmd.batch [ syncProfile, syncBoards ] )


finishModification : Model -> Model
finishModification model =
    -- TODO: fogure out how to sync this to the backend
    case model.renamingState of
        RenamingItem { itemId, newName } ->
            { model
                | renamingState = NoRename
                , boards = updateName model.boards itemId newName
                , stacks = updateName model.stacks itemId newName
            }

        NoRename ->
            model


updateName : Dict String { a | name : String } -> String -> String -> Dict String { a | name : String }
updateName boards boardId newName =
    Dict.update boardId (Maybe.map (\s -> { s | name = newName })) boards


decodeItems : Json.Decoder SearchResponse
decodeItems =
    Json.map SearchResponse
        (Json.field "items" (Json.list mapItem))


mapItem : Json.Decoder Item
mapItem =
    Json.map3 Item
        (Json.field "id" Json.string)
        (Json.field "name" Json.string)
        (Json.field "youtubeId" Json.string)


createId =
    Random.float 0 1 |> Random.map String.fromFloat


updateBoard : String -> (Board -> Board) -> Dict String Board -> Dict String Board
updateBoard boardId updater boards =
    Dict.update boardId (Maybe.map (\v -> updater v)) boards



-- VIEW


view : Model -> Maybe Login.LoginSuccessResponse -> Html Msg
view model login =
    case model.userProfile.selectedBoard of
        -- ugly assumption, but works for now. Consider using a separate precise state of loading
        -- maybe even load your state progressively
        "" ->
            div [] [ text "Loading user profile..." ]

        _ ->
            div
                [ attributeIf (model.dragState /= NoDrag) (onMouseMove MouseMove)
                , attributeIf (model.dragState /= NoDrag) (onMouseUp MouseUp)
                , classIf (isDraggingAnything model.dragState) "board-during-drag"
                ]
                [ viewTopBar model login
                , div []
                    [ viewSidebar model
                    , viewBoard model
                    ]
                , viewPlayer model
                , viewElementBeingDragged model
                ]


viewTopBar : Model -> Maybe Login.LoginSuccessResponse -> Html Msg
viewTopBar model login =
    div [ class "top-bar" ]
        [ div []
            [ button [ classIf (model.sidebarState == Boards) "active", onClick (SetSidebar Boards) ] [ text "boards" ]
            , button [ classIf (model.sidebarState == Search) "active", onClick (SetSidebar Search) ] [ text "search" ]
            ]
        , Maybe.map viewUser login |> Maybe.withDefault (div [] [])
        ]


viewUser : Login.LoginSuccessResponse -> Html Msg
viewUser loginInfo =
    div [ class "user-info-container" ]
        [ button [ onClick SaveModifiedItemsOnDemand ] [ text "save" ]
        , span [] [ text loginInfo.displayName ]
        , img [ class "user-info-image", src loginInfo.photoURL ] []
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    case getBoardViewModel model of
        Just board ->
            div
                [ class "board"
                , classIf (model.sidebarState /= Hidden) "board-with-sidebar"
                ]
                [ viewBoardBar board
                , div
                    [ class "columns-container" ]
                    (List.append
                        (board.children
                            |> List.map (\stackId -> Dict.get stackId model.stacks)
                            |> ignoreNothing
                            |> List.map (\stack -> viewStack model.renamingState model.dragState [ class "column-board" ] (getStackToView model stack.id))
                        )
                        [ button [ class "add-stack-button", onClick CreateSingleId ] [ text "add" ] ]
                    )
                ]

        Nothing ->
            div [] [ text "Loading BOARD" ]


viewBoardBar : Board -> Html Msg
viewBoardBar { name } =
    div [ class "board-header" ] [ text name ]


viewStack : RenamingState -> DragState -> List (Attribute Msg) -> ( Stack, List Item ) -> Html Msg
viewStack modificationState dragState attributes ( { id, name }, items ) =
    div (List.append [ class "column-drag-overlay" ] attributes)
        [ div
            [ class "column"
            , classIf (isDraggingStack dragState id) "column-preview"
            , onMouseEnter (StackEnterDuringDrag id)
            ]
            [ div [ class "column-title", onMouseDown (StackTitleMouseDown id) ]
                [ viewContent modificationState { id = id, name = name }
                , div [ class "column-title-actions" ]
                    [ button [ onMouseDownAlwaysStopPropagation (StartModifyingItem { itemId = id, newName = name }) ] [ text "E" ]
                    , button [ onMouseDownAlwaysStopPropagation (RemoveStack id) ] [ text "X" ]
                    ]
                ]
            , div [ class "column-content" ]
                (if List.isEmpty items then
                    [ div [ class "empty-stack-placeholder", onMouseEnter (StackOverlayEnterDuringDrag id) ] [] ]

                 else
                    List.map (\item -> viewItem [] (isDraggingItem dragState item) item) items
                )
            ]
        , div [ class "column-footer", onMouseEnter (StackOverlayEnterDuringDrag id) ] []
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    case model.sidebarState of
        Search ->
            div [ class "sidebar sidebar-padded" ]
                (viewSearch model)

        Boards ->
            div [ class "sidebar" ]
                (viewBoards model)

        Hidden ->
            div [] []


viewSearch model =
    let
        stackM =
            getSearchStack model

        items =
            case stackM of
                Just stack ->
                    stack.children |> List.map (\itemId -> Dict.get itemId model.items) |> ignoreNothing

                Nothing ->
                    []
    in
    [ div [ class "sidebar-header" ] [ h3 [] [ text "Search" ], button [ onClick (SetSidebar Hidden) ] [ text "<" ] ]
    , input [ onInput OnSearchInput, placeholder "Find videos by name...", value model.searchTerm ] []
    , div [] (List.map (\item -> viewItem [] (isDraggingItem model.dragState item) item) items)
    ]


viewBoards model =
    [ div [ class "sidebar-header sidebar-padded" ]
        [ h3 [] [ text "Boards" ]
        , button [ onClick (SetSidebar Hidden) ] [ text "<" ]
        ]
    , div []
        (model.userProfile.boards
            |> List.map (flipArguments Dict.get model.boards)
            |> ignoreNothing
            |> List.map (\item -> viewBoardButton model (isDraggingBoard model.dragState item.id) [] item)
        )
    , div [] [ button [ onClick CreateNewBoard ] [ text "Add" ] ]
    ]


viewBoardButton : Model -> Bool -> List (Attribute Msg) -> Board -> Html Msg
viewBoardButton model isDragging attrs item =
    div
        (List.append
            [ class "sidebar-boards-button"
            , classIf (model.userProfile.selectedBoard == item.id) "active"
            , onClickIf (not (isDraggingAnyBoard model.dragState)) (SelectBoard item.id)
            , onMouseDown (BoardMouseDown item.id)
            , onMouseEnter (BoardEnterDuringDrag item.id)
            , classIf isDragging "item-preview"
            ]
            attrs
        )
        [ viewContent model.renamingState item
        , div [ class "sidebar-boards-button-actions" ]
            [ button [ onClickAlwaysStopPropagation (StartModifyingItem { itemId = item.id, newName = item.name }) ] [ text "E" ]
            , button [ onClickAlwaysStopPropagation (RemoveBoard item.id) ] [ text "X" ]
            ]
        ]


viewContent modificationState { name, id } =
    case modificationState of
        RenamingItem { itemId, newName } ->
            if itemId == id then
                input [ onBlur ApplyModification, Attributes.id id, value newName, onInput OnNewNameEnter, onKeyUp OnModificationKeyUp ] []

            else
                text name

        NoRename ->
            text name


viewItem : List (Attribute Msg) -> Bool -> Item -> Html Msg
viewItem atts isDragging { id, name, youtubeId } =
    div
        (List.append
            [ class "item"
            , classIf isDragging "item-preview"
            ]
            atts
        )
        [ img [ draggable "false", class "item-image", src ("https://i.ytimg.com/vi/" ++ youtubeId ++ "/mqdefault.jpg") ]
            []
        , span [ class "item-text" ] [ text name ]
        , div [ class "item-click-overlay", onMouseDown (ItemMouseDown id), onMouseEnter (ItemEnterDuringDrag id) ] []
        ]


viewElementBeingDragged model =
    case model.dragState of
        DraggingItem mouseMoveEvent offsets itemId ->
            viewItem
                [ class "item-dragged"
                , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                ]
                False
                (model.items |> Dict.get itemId |> Maybe.withDefault (Item "1" "1" "1"))

        DraggingStack mouseMoveEvent offsets stackId ->
            let
                ( stack, items ) =
                    getStackToView model stackId
            in
            viewStack NoRename
                NoDrag
                [ class "item-dragged"
                , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                ]
                ( stack, items )

        DraggingBoard mouseMoveEvent offsets boardId ->
            case model.boards |> Dict.get boardId of
                Just actualBoard ->
                    viewBoardButton model
                        False
                        [ class "item-dragged"
                        , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                        , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                        ]
                        actualBoard

                Nothing ->
                    div [] []

        NoDrag ->
            div [] []

        ItemPressedNotYetMoved _ _ _ ->
            div [] []

        BoardPressedNotYetMoved _ _ _ ->
            div [] []


viewPlayer model =
    case model.videoBeingPlayed of
        Just videoId ->
            let
                item =
                    getItemById videoId model
            in
            case item of
                Just actualItem ->
                    div [ class "player-container" ]
                        [ Embed.Youtube.fromString actualItem.youtubeId
                            |> Embed.Youtube.attributes
                                [ Embed.Youtube.Attributes.width 400
                                , Embed.Youtube.Attributes.height 150
                                , Embed.Youtube.Attributes.autoplay
                                , Embed.Youtube.Attributes.modestBranding
                                ]
                            |> Embed.Youtube.toHtml
                        ]

                Nothing ->
                    div [] []

        Nothing ->
            div [] []
