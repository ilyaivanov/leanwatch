port module BoardPage exposing (Model, Msg(..), createBoard, init, onBoardCreated, onBoardsLoaded, onUserProfileLoaded, onVideoEnded, update, view)

import Board exposing (..)
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
import Utils.Other exposing (flip, getNextItem, ifNothing, maybeHasValue, removeItem, unpackMaybes)


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
        | board =
            { boards = Dict.insert boardResponse.id { id = boardResponse.id, name = boardResponse.name, children = getIds boardResponse.stacks } model.board.boards
            , stacks = Dict.union stacks model.board.stacks
            , items = Dict.union items model.board.items
            }
    }


denormalizeBoard : String -> Model -> Maybe BoardResponse
denormalizeBoard boardId model =
    Dict.get boardId model.board.boards
        |> Maybe.map
            (\board ->
                let
                    stacksNarrow =
                        board.children |> List.map (\id -> Dict.get id model.board.stacks) |> unpackMaybes

                    stacks =
                        stacksNarrow |> List.map (\s -> { id = s.id, name = s.name, items = s.children |> List.map (\id -> Dict.get id model.board.items) |> unpackMaybes })
                in
                { id = board.id
                , name = board.name
                , stacks = stacks
                }
            )



-- MODEL


getBoardViewModel : Model -> Maybe Board
getBoardViewModel model =
    Dict.get model.userProfile.selectedBoard model.board.boards


type alias Model =
    { board : BoardModel
    , videoBeingPlayed : Maybe String
    , userProfile : UserProfile

    -- UI STATE
    , sidebarState : SidebarState
    , dragState : DragState
    , renamingState : RenamingState
    , boardIdsToSync : Set String
    , needToSyncProfile : Bool
    , searchTerm : String
    , isWaitingForSearch : Bool
    , isLoadingMoreItems : Bool
    , isLoadingSimilar : Bool
    , searchNextPageToken : String
    , playerMode : PlayerMode

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
    | Similar


type PlayerMode
    = MiniPlayer
    | CinemaPlayer


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
    | GotItems Bool String (Result Http.Error SearchResponse)
    | LoadMoreSearch
    | SearchSimilar Item
      -- Sidebar
    | SetSidebar SidebarState
      -- Board Management
    | SelectBoard String
    | UserProfileLoaded UserProfile
    | BoardsLoaded (List BoardResponse)
    | OnBoardCreated BoardResponse
    | CreateNewBoard
    | TogglePlayerMode
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
    { board = Board.init
    , userProfile =
        { selectedBoard = ""
        , boards = []
        , id = ""
        , syncTime = oneMinute
        }
    , boardIdsToSync = Set.empty
    , needToSyncProfile = False
    , isWaitingForSearch = False
    , isLoadingMoreItems = False
    , isLoadingSimilar = False
    , searchNextPageToken = ""
    , dragState = DragState.init
    , renamingState = NoRename
    , searchTerm = ""
    , currentSearchId = ""
    , videoBeingPlayed = Nothing
    , sidebarState = Boards
    , playerMode = MiniPlayer
    }


type alias UserProfile =
    { id : String
    , boards : List String
    , selectedBoard : String
    , syncTime : Float
    }


type alias SearchResponse =
    { items : List Item
    , nextPageToken : String
    }



-- UPDATE


updateBoard model board =
    { model | board = board }


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
                    item
                        |> Maybe.andThen (getItemById model.board)
                        |> Maybe.map .youtubeId
                        |> Maybe.map play
                        |> Maybe.withDefault Cmd.none

                nextVideo =
                    ifNothing item model.videoBeingPlayed
            in
            ( { model | dragState = nextDragState, videoBeingPlayed = nextVideo }, nextCommand )

        MouseMove newMousePosition ->
            noComand { model | dragState = handleMouseMove model.dragState newMousePosition }

        ItemEnterDuringDrag itemUnder ->
            handleCardEnter model.dragState itemUnder model.board.stacks
                |> Maybe.map (\newStacks -> { model | board = setStacks newStacks model.board })
                |> Maybe.map markSelectedBoardAsNeededToSync
                |> Maybe.withDefault model
                |> noComand

        StackEnterDuringDrag stackUnder ->
            handleStackEnter model.dragState stackUnder model.board.boards
                |> Maybe.map (\newBoards -> { model | board = setBoards newBoards model.board })
                |> Maybe.map markSelectedBoardAsNeededToSync
                |> Maybe.withDefault model
                |> noComand

        StackOverlayEnterDuringDrag stackUnder ->
            handleStackOverlayEnter model.dragState stackUnder model.board
                |> Maybe.map (updateBoard model)
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
            { model | board = createStack model.userProfile.selectedBoard newStackId model.board }
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
                        , expect = Http.expectJson (GotItems False "SEARCH") decodeItems
                        }
            in
            if id == model.currentSearchId then
                ( { model | currentSearchId = "", isWaitingForSearch = True }, request )

            else
                ( model, Cmd.none )

        GotItems needToAppend stackType response ->
            case response of
                Ok body ->
                    let
                        nextModel =
                            if needToAppend then
                                appendStackChildren stackType body.items model.board

                            else
                                setStackChildren stackType body.items model.board
                    in
                    noComand
                        { model
                            | board = nextModel

                            --This is wrong - won't handle several parallel requests to similar/search
                            , isWaitingForSearch = False
                            , isLoadingMoreItems = False
                            , isLoadingSimilar = False
                            , searchNextPageToken = body.nextPageToken
                        }

                _ ->
                    -- Handle errors
                    noComand { model | isWaitingForSearch = False }

        LoadMoreSearch ->
            let
                request =
                    Http.get
                        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?q=" ++ model.searchTerm ++ "&pageToken=" ++ model.searchNextPageToken
                        , expect = Http.expectJson (GotItems True "SEARCH") decodeItems
                        }
            in
            ( { model | isLoadingMoreItems = True }, request )

        SearchSimilar item ->
            let
                request =
                    Http.get
                        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?relatedToVideoId=" ++ item.youtubeId ++ "&type=video"
                        , expect = Http.expectJson (GotItems False "SIMILAR") decodeItems
                        }
            in
            ( { model | isLoadingSimilar = True, sidebarState = Similar }, request )

        SaveModifiedItemsOnDemand ->
            saveModifiedItems model

        SaveModifiedItemsScheduled ->
            let
                ( newModel, cmds ) =
                    saveModifiedItems model
            in
            ( newModel, Cmd.batch [ cmds, scheduleNextSync model.userProfile.syncTime ] )

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
            noComand ({ model | board = removeBoard boardId model.board } |> updateProfileAndMarkAsNeededToSync newProfile)

        RemoveStack stackId ->
            { model | board = removeStack model.userProfile.selectedBoard stackId model.board } |> noComand

        VideoEnded _ ->
            case getNextItemInStack model.videoBeingPlayed model.board of
                Just nextItem ->
                    ( { model | videoBeingPlayed = Just nextItem.id }, play nextItem.youtubeId )

                Nothing ->
                    ( model, Cmd.none )

        TogglePlayerMode ->
            let
                nextPlayerState =
                    if model.playerMode == CinemaPlayer then
                        MiniPlayer

                    else
                        CinemaPlayer
            in
            { model | playerMode = nextPlayerState } |> noComand

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
                , board = updateName itemId newName model.board
            }
                |> markSelectedBoardAsNeededToSync

        NoRename ->
            model


decodeItems : Json.Decoder SearchResponse
decodeItems =
    Json.map2 SearchResponse
        (Json.field "items" (Json.list mapItem))
        (Json.field "nextPageToken" Json.string)


mapItem : Json.Decoder Item
mapItem =
    Json.map3 Item
        (Json.field "id" Json.string)
        (Json.field "name" Json.string)
        (Json.field "youtubeId" Json.string)


createId =
    Random.float 0 1 |> Random.map String.fromFloat



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
                , viewElementBeingDragged model
                ]


viewTopBar : Model -> Maybe Login.LoginSuccessResponse -> Html Msg
viewTopBar model login =
    div [ class "top-bar" ]
        [ div []
            [ button [ classIf (model.sidebarState == Boards) "active", onClick (SetSidebar Boards) ] [ text "Boards" ]
            , button [ classIf (model.sidebarState == Search) "active", onClick (SetSidebar Search) ] [ text "Search" ]
            , button [ classIf (model.sidebarState == Similar) "active", onClick (SetSidebar Similar) ] [ text "Similar" ]
            ]
        , div []
            [ button [ onClick SaveModifiedItemsOnDemand ] [ text "Save" ]
            , button [ onClick TogglePlayerMode ]
                [ if model.playerMode == CinemaPlayer then
                    text "Mini-player Mode"

                  else
                    text "Cinema Mode"
                ]
            ]
        , Maybe.map viewUser login |> Maybe.withDefault (div [] [])
        ]


viewPlayer model =
    div
        [ classIf (model.sidebarState /= Hidden) "board-with-sidebar"
        , classIf (hasCinemaVideo model) "cinema-player"
        , classIf (model.playerMode == MiniPlayer) "mini-player"
        ]
        [ div [ id "youtubePlayer" ] [] ]


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
                [ viewPlayer model
                , viewBoardBar board
                , div
                    [ class "columns-container" ]
                    (List.append
                        (board.children
                            |> List.map (\stackId -> Dict.get stackId model.board.stacks)
                            |> unpackMaybes
                            |> List.map (\stack -> viewStack model [ class "column-board" ] (getStackToView model.board stack.id))
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
            div [ class "sidebar" ]
                (viewSearch model)

        Boards ->
            div [ class "sidebar" ]
                (viewBoards model)

        Similar ->
            div [ class "sidebar" ]
                (viewSimilar model)

        Hidden ->
            div [] []


hasCinemaVideo model =
    model.playerMode == CinemaPlayer && maybeHasValue model.videoBeingPlayed


viewSearch model =
    let
        items =
            getItemsForStack "SEARCH" model.board
    in
    [ viewSidebarHeader "Search"
    , input [ class "sidebar-search-input", onInput OnSearchInput, placeholder "Find videos by name...", value model.searchTerm ] []
    , div [] (List.map (\item -> viewItem [] model.dragState model.videoBeingPlayed item) items)
    , elementIf model.isWaitingForSearch (progress [ class "material-progress-linear top" ] [])
    , elementIf (not (List.isEmpty items) && not model.isLoadingMoreItems)
        (div [ class "sidebar-bottom-action-container" ] [ button [ onClick LoadMoreSearch, class "dark" ] [ text "load more" ] ])
    , elementIf model.isLoadingMoreItems (progress [ class "material-progress-linear" ] [])
    ]


viewSimilar model =
    let
        items =
            getItemsForStack "SIMILAR" model.board
    in
    [ elementIf model.isLoadingSimilar (progress [ class "material-progress-linear top" ] [])
    , viewSidebarHeader "Similar"
    , div [] (List.map (\item -> viewItem [] model.dragState model.videoBeingPlayed item) items)
    ]


viewSidebarHeader title =
    div [ class "sidebar-header" ] [ h3 [] [ text title ], button [ onClick (SetSidebar Hidden), class "icon-button hide-icon" ] [ img [ src "/icons/chevron.svg" ] [] ] ]


elementIf condition element =
    if condition then
        element

    else
        div [] []


viewBoards model =
    [ viewSidebarHeader ("Boards" ++ asteriskIf model.needToSyncProfile)
    , viewSyncMessage model.userProfile
    , div []
        (model.userProfile.boards
            |> List.map (flip Dict.get model.board.boards)
            |> unpackMaybes
            |> List.map (\item -> viewBoardButton model.dragState model [] item)
        )
    , div [ class "sidebar-bottom-action-container" ] [ button [ onClick CreateNewBoard, class "dark" ] [ text "Add board" ] ]
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
viewItem atts dragState videoBeingPlayed item =
    div
        (List.append
            [ class "item"
            , classIf (isDraggingItem dragState item.id) "item-preview"
            , classIf (Maybe.withDefault "" videoBeingPlayed == item.id) "active"
            ]
            atts
        )
        [ img [ draggable "false", class "item-image", src ("https://i.ytimg.com/vi/" ++ item.youtubeId ++ "/mqdefault.jpg") ]
            []
        , span [ class "item-text" ] [ text item.name ]
        , div [ class "item-click-overlay", onMouseEnter (ItemEnterDuringDrag item.id) ]
            [ div [ class "click1", onMouseDown (ItemMouseDown item.id) ] []
            , div [ class "item-icon-area", onClick (SearchSimilar item) ] [ img [ draggable "false", src "/icons/similar.svg" ] [] ]
            ]
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
            Dict.get itemId model.board.items
                |> Maybe.map (viewItem (getAttributes elementPosition) DragState.init model.videoBeingPlayed)
                |> Maybe.withDefault (div [] [])

        Just ( StackBeingDragged, elementPosition, stackId ) ->
            viewStack { renamingState = NoRename, dragState = DragState.init, videoBeingPlayed = model.videoBeingPlayed } (getAttributes elementPosition) (getStackToView model.board stackId)

        Just ( BoardBeingDragged, elementPosition, boardId ) ->
            model.board.boards
                |> Dict.get boardId
                |> Maybe.map (viewBoardButton DragState.init model (getAttributes elementPosition))
                |> Maybe.withDefault (div [] [])

        Nothing ->
            div [] []
