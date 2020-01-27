port module Board exposing (Item, Model, Msg(..), Stack, createBoard, init, onBoardCreated, onBoardsLoaded, onUserProfileLoaded, onVideoEnded, update, view)

import Browser.Dom as Dom exposing (focus)
import Dict exposing (Dict)
import DictMoves exposing (Parent, getParentByChildren)
import DragState exposing (..)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput)
import Http
import Json.Decode as Json
import Login
import Process
import Random
import Set exposing (Set)
import Task
import Utils.ExtraEvents exposing (..)
import Utils.Other exposing (flip, getNextItem, ifNothing, removeItem, unpackMaybes)


noComand : Model -> ( Model, Cmd msg )
noComand model =
    ( model, Cmd.none )


port onUserProfileLoaded : (UserProfile -> msg) -> Sub msg


port onBoardsLoaded : (List BoardResponse -> msg) -> Sub msg


port saveBoard : List BoardResponse -> Cmd msg


port saveProfile : UserProfile -> Cmd msg


port createBoard : () -> Cmd msg


port logout : () -> Cmd msg


port play : String -> Cmd msg


port onVideoEnded : (() -> msg) -> Sub msg


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
                        board.children |> List.map (\id -> Dict.get id model.stacks) |> unpackMaybes

                    stacks =
                        stacksNarrow |> List.map (\s -> { id = s.id, name = s.name, items = s.children |> List.map (\id -> Dict.get id model.items) |> unpackMaybes })
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
    | VideoEnded ()
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
      -- Login
    | Logout


oneMinute =
    1000 * 60


init : Model
init =
    { stacks = Dict.empty
    , items = Dict.empty
    , boards = Dict.empty
    , userProfile =
        { selectedBoard = ""
        , boards = []
        , id = ""
        , syncTime = oneMinute
        }
    , boardIdsToSync = Set.empty
    , needToSyncProfile = False
    , dragState = DragState.init
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
    , syncTime : Float
    }


type alias Item =
    { id : String
    , name : String
    , youtubeId : String
    }


type alias SearchResponse =
    { items : List Item
    }


getSearchStack : Model -> Maybe Stack
getSearchStack model =
    model.stacks |> Dict.get "SEARCH"


getStackToView : Model -> String -> ( Stack, List Item )
getStackToView model stackId =
    let
        stack =
            getStack stackId model.stacks

        items =
            stack.children |> List.map (\itemId -> Dict.get itemId model.items) |> unpackMaybes
    in
    ( stack, items )


getItemById : Model -> String -> Maybe Item
getItemById model itemId =
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


updateBoardsInProfile profile boards =
    { profile | boards = boards }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemMouseDown itemId event ->
            noComand { model | dragState = handleCardMouseDown itemId event }

        BoardMouseDown boardId event ->
            noComand { model | dragState = handleBoardMouseDown boardId event }

        StackTitleMouseDown stackId event ->
            noComand { model | dragState = handleStackTitleMouseDown stackId event }

        MouseUp ->
            let
                ( nextDragState, item ) =
                    handleMouseUp model.dragState

                nextCommand =
                    item |> Maybe.andThen (getItemById model) |> Maybe.map .youtubeId |> Maybe.map play |> Maybe.withDefault Cmd.none

                nextVideo =
                    ifNothing item model.videoBeingPlayed
            in
            ( { model | dragState = nextDragState, videoBeingPlayed = nextVideo }, nextCommand )

        MouseMove newMousePosition ->
            noComand { model | dragState = handleMouseMove model.dragState newMousePosition }

        ItemEnterDuringDrag itemUnder ->
            handleCardEnter model.dragState itemUnder model.stacks
                |> Maybe.map (\newStacks -> { model | stacks = newStacks })
                |> Maybe.map markSelectedBoardAsNeededToSync
                |> Maybe.withDefault model
                |> noComand

        StackEnterDuringDrag stackUnder ->
            handleStackEnter model.dragState stackUnder model.boards
                |> Maybe.map (\newBoards -> { model | boards = newBoards })
                |> Maybe.map markSelectedBoardAsNeededToSync
                |> Maybe.withDefault model
                |> noComand

        StackOverlayEnterDuringDrag stackUnder ->
            handleStackOverlayEnter model.dragState stackUnder model
                |> Maybe.map markSelectedBoardAsNeededToSync
                |> Maybe.withDefault model
                |> noComand

        BoardEnterDuringDrag boardUnder ->
            handleBoardEnter model.dragState boardUnder model.userProfile.boards
                |> Maybe.map (updateBoardsInProfile model.userProfile)
                |> Maybe.map (flip updateProfileAndMarkAsNeededToSync model)
                |> Maybe.withDefault model
                |> noComand

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
                        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?q=" ++ term
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
            ( newModel, Cmd.batch [ cmds, scheduleNextSync model.userProfile.syncTime ] )

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
            ( { model | userProfile = profile }, scheduleNextSync model.userProfile.syncTime )

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
            ( mergeAndNormalizeResponse boardResponse model |> updateProfileAndMarkAsNeededToSync newProfile |> markSelectedBoardAsNeededToSync, Cmd.none )

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

        VideoEnded _ ->
            let
                stack =
                    getParentByChildren (model.videoBeingPlayed |> Maybe.withDefault "") model.stacks
            in
            case ( stack, model.videoBeingPlayed ) of
                ( Just actualStack, Just videoPlayed ) ->
                    case getNextItem videoPlayed actualStack.children of
                        Just nextItemId ->
                            case getItemById model nextItemId of
                                Just nextItem ->
                                    ( { model | videoBeingPlayed = Just nextItem.id }, play nextItem.youtubeId )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Noop ->
            noComand model

        Logout ->
            ( model, logout () )

        FocusResult _ ->
            noComand model


scheduleNextSync time =
    Process.sleep time |> Task.perform (always SaveModifiedItemsScheduled)


saveModifiedItems : Model -> ( Model, Cmd Msg )
saveModifiedItems model =
    let
        syncProfile =
            if model.needToSyncProfile then
                saveProfile model.userProfile

            else
                Cmd.none

        boardsToSync =
            Set.toList model.boardIdsToSync |> List.map (\boardId -> denormalizeBoard boardId model) |> unpackMaybes

        syncBoardsCmd =
            if List.isEmpty boardsToSync then
                Cmd.none

            else
                saveBoard boardsToSync
    in
    ( { model | needToSyncProfile = False, boardIdsToSync = Set.empty }, Cmd.batch [ syncProfile, syncBoardsCmd ] )


finishModification : Model -> Model
finishModification model =
    case model.renamingState of
        RenamingItem { itemId, newName } ->
            { model
                | renamingState = NoRename
                , boards = updateName model.boards itemId newName
                , stacks = updateName model.stacks itemId newName
            }
                |> markSelectedBoardAsNeededToSync

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
                [ attributeIf (shouldListenToMouseMoveEvents model.dragState) (onMouseMove MouseMove)
                , attributeIf (shouldListenToMouseMoveEvents model.dragState) (onMouseUp MouseUp)
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
            [ button [ classIf (model.sidebarState == Boards) "active", onClick (SetSidebar Boards) ] [ text "Boards" ]
            , button [ classIf (model.sidebarState == Search) "active", onClick (SetSidebar Search) ] [ text "Search" ]
            ]
        , button [ onClick SaveModifiedItemsOnDemand ] [ text "Save" ]
        , Maybe.map viewUser login |> Maybe.withDefault (div [] [])
        ]


viewUser : Login.LoginSuccessResponse -> Html Msg
viewUser loginInfo =
    div [ class "user-info-container" ]
        [ button [ onClick Logout ] [ text "Logout" ]
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
                            |> unpackMaybes
                            |> List.map (\stack -> viewStack model [ class "column-board" ] (getStackToView model stack.id))
                        )
                        [ button [ class "add-stack-button", onClick CreateSingleId ] [ text "Add column" ], div [ class "post-add-stack-space" ] [] ]
                    )
                ]

        Nothing ->
            div [] [ text "Loading BOARD" ]


viewBoardBar : Board -> Html Msg
viewBoardBar { name } =
    div [ class "board-title" ] [ text name ]


viewStack : { a | renamingState : RenamingState, dragState : DragState, videoBeingPlayed : Maybe String } -> List (Attribute Msg) -> ( Stack, List Item ) -> Html Msg
viewStack { renamingState, dragState, videoBeingPlayed } attributes ( { id, name }, items ) =
    div (List.append [ class "column-drag-overlay" ] attributes)
        [ div
            [ class "column"
            , classIf (isDraggingItem dragState id) "column-preview"
            , onMouseEnter (StackEnterDuringDrag id)
            ]
            [ div [ class "column-title", onMouseDown (StackTitleMouseDown id) ]
                [ viewContent renamingState { id = id, name = name }
                , div [ class "column-title-actions" ]
                    [ button [ onMouseDownAlwaysStopPropagation (StartModifyingItem { itemId = id, newName = name }), class "icon-button" ] [ img [ src "/icons/edit.svg" ] [] ]
                    , button [ onMouseDownAlwaysStopPropagation (RemoveStack id), class "icon-button" ] [ img [ src "/icons/delete.svg" ] [] ]
                    ]
                ]
            , div [ class "column-content" ]
                (if List.isEmpty items then
                    [ div [ class "empty-stack-placeholder", onMouseEnter (StackOverlayEnterDuringDrag id) ] [] ]

                 else
                    List.map (\item -> viewItem [] dragState videoBeingPlayed item) items
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
                    stack.children |> List.map (\itemId -> Dict.get itemId model.items) |> unpackMaybes

                Nothing ->
                    []
    in
    [ div [ class "sidebar-header" ] [ h3 [] [ text "Search" ], button [ onClick (SetSidebar Hidden), class "icon-button hide-icon" ] [ img [ src "/icons/chevron.svg" ] [] ] ]
    , input [ class "sidebar-search-input", onInput OnSearchInput, placeholder "Find videos by name...", value model.searchTerm ] []
    , div [] (List.map (\item -> viewItem [] model.dragState model.videoBeingPlayed item) items)
    ]


viewBoards model =
    [ div [ class "sidebar-header" ]
        [ h3 [] [ text ("Boards" ++ asteriskIf model.needToSyncProfile), viewSyncMessage model.userProfile ]
        , button [ onClick (SetSidebar Hidden), class "icon-button hide-icon" ] [ img [ src "/icons/chevron.svg" ] [] ]
        ]
    , div []
        (model.userProfile.boards
            |> List.map (flip Dict.get model.boards)
            |> unpackMaybes
            |> List.map (\item -> viewBoardButton model.dragState model [] item)
        )
    , div [ class "add-board-container" ] [ button [ onClick CreateNewBoard, class "dark" ] [ text "Add board" ] ]
    , div [ class "small-text" ] [ text "* - means board requires syncing" ]
    ]


viewSyncMessage profile =
    div []
        [ div [ class "small-text" ] [ text ("Saving boards every " ++ toInt profile.syncTime ++ " seconds") ]
        , div [ class "small-text" ] [ text "Press 'Save' to force saving" ]
        ]


toInt x =
    String.fromInt (round (x / 1000))


viewBoardButton : DragState -> Model -> List (Attribute Msg) -> Board -> Html Msg
viewBoardButton dragState model attrs item =
    div
        (List.append
            [ class "sidebar-boards-button"
            , classIf (model.userProfile.selectedBoard == item.id) "active"
            , onClickIf (not (isDraggingAnyBoard dragState)) (SelectBoard item.id)
            , onMouseDown (BoardMouseDown item.id)
            , onMouseEnter (BoardEnterDuringDrag item.id)
            , classIf (isDraggingItem dragState item.id) "item-preview"
            ]
            attrs
        )
        [ viewContent model.renamingState { id = item.id, name = item.name ++ asteriskIf (Set.member item.id model.boardIdsToSync) }
        , div [ class "sidebar-boards-button-actions" ]
            [ button [ onClickAlwaysStopPropagation (StartModifyingItem { itemId = item.id, newName = item.name }), class "icon-button" ]
                [ img [ src "/icons/edit.svg" ] [] ]
            , button [ onClickAlwaysStopPropagation (RemoveBoard item.id), class "icon-button" ]
                [ img [ src "/icons/delete.svg" ] [] ]
            ]
        ]


asteriskIf condition =
    if condition then
        "*"

    else
        ""


viewContent modificationState { name, id } =
    case modificationState of
        RenamingItem { itemId, newName } ->
            if itemId == id then
                input [ onBlur ApplyModification, Attributes.id id, value newName, onInput OnNewNameEnter, onKeyUp OnModificationKeyUp ] []

            else
                text name

        NoRename ->
            text name


viewItem : List (Attribute Msg) -> DragState -> Maybe String -> Item -> Html Msg
viewItem atts dragState videoBeingPlayed { id, name, youtubeId } =
    div
        (List.append
            [ class "item"
            , classIf (isDraggingItem dragState id) "item-preview"
            , classIf (Maybe.withDefault "" videoBeingPlayed == id) "active"
            ]
            atts
        )
        [ img [ draggable "false", class "item-image", src ("https://i.ytimg.com/vi/" ++ youtubeId ++ "/mqdefault.jpg") ]
            []
        , span [ class "item-text" ] [ text name ]
        , div [ class "item-click-overlay", onMouseDown (ItemMouseDown id), onMouseEnter (ItemEnterDuringDrag id) ] []
        ]


viewElementBeingDragged : Model -> Html Msg
viewElementBeingDragged model =
    let
        getAttributes elementPosition =
            [ class "item-dragged"
            , style "left" (String.fromInt elementPosition.left ++ "px")
            , style "top" (String.fromInt elementPosition.top ++ "px")
            ]
    in
    case getItemBeingDragged model.dragState of
        Just ( CardBeingDragged, elementPosition, itemId ) ->
            Dict.get itemId model.items
                |> Maybe.map (viewItem (getAttributes elementPosition) DragState.init model.videoBeingPlayed)
                |> Maybe.withDefault (div [] [])

        Just ( StackBeingDragged, elementPosition, stackId ) ->
            viewStack { renamingState = NoRename, dragState = DragState.init, videoBeingPlayed = model.videoBeingPlayed } (getAttributes elementPosition) (getStackToView model stackId)

        Just ( BoardBeingDragged, elementPosition, boardId ) ->
            model.boards
                |> Dict.get boardId
                |> Maybe.map (viewBoardButton DragState.init model (getAttributes elementPosition))
                |> Maybe.withDefault (div [] [])

        Nothing ->
            div [] []


viewPlayer model =
    div [ class "player-container" ]
        [ div [ id "youtubePlayer" ] []
        ]
