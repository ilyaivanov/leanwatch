port module BoardPage exposing (Model, Msg(..), createBoard, init, onBoardCreated, onBoardsLoaded, onUserProfileLoaded, onVideoEnded, onVideoProgress, update, view)

import Board exposing (..)
import Browser.Dom as Dom exposing (focus)
import Dict exposing (Dict)
import DictMoves exposing (Parent)
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
import Utils.Other exposing (..)


noComand : Model -> ( Model, Cmd msg )
noComand model =
    ( model, Cmd.none )


port onUserProfileLoaded : (UserProfile -> msg) -> Sub msg


port onBoardsLoaded : (Json.Value -> msg) -> Sub msg


port saveBoard : List BoardResponse -> Cmd msg


port saveProfile : UserProfile -> Cmd msg


port createBoard : () -> Cmd msg


port logout : () -> Cmd msg


port play : String -> Cmd msg


port onVideoEnded : (() -> msg) -> Sub msg


port onBoardCreated : (BoardResponse -> msg) -> Sub msg


port onVideoProgress : (Json.Value -> msg) -> Sub msg



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
    , playerMode : PlayerMode
    , similarItem : Maybe Item

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
    | UpdateTimeline TimeLineInfo
      -- Debounced search
    | AttemptToSearch String
    | DebouncedSearch String String
    | FinishLoadingItems String (Result Http.Error SearchResponse)
    | FinishLoadingPage String (Result Http.Error SearchResponse)
    | LoadMoreSearch String
    | LoadMoreSimilar String
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
    , dragState = DragState.init
    , renamingState = NoRename
    , searchTerm = ""
    , currentSearchId = ""
    , videoBeingPlayed = Nothing
    , similarItem = Nothing
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
                        , expect = Http.expectJson (FinishLoadingItems "SEARCH") decodeItems
                        }
            in
            if id == model.currentSearchId then
                ( { model | currentSearchId = "", board = startLoading "SEARCH" model.board }, request )

            else
                ( model, Cmd.none )

        FinishLoadingItems stackId response ->
            case response of
                Ok body ->
                    let
                        nextBoard =
                            model.board |> setStackChildren stackId body.items |> onStackLoadingDone stackId body.nextPageToken
                    in
                    { model | board = nextBoard } |> noComand

                Err error ->
                    model |> noComand

        FinishLoadingPage stackId response ->
            case response of
                Ok body ->
                    let
                        nextBoard =
                            model.board |> appendStackChildren stackId body.items |> onStackLoadingDone stackId body.nextPageToken
                    in
                    { model | board = nextBoard } |> noComand

                Err error ->
                    model |> noComand

        LoadMoreSearch nextPage ->
            let
                request =
                    Http.get
                        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?q=" ++ model.searchTerm ++ "&pageToken=" ++ nextPage
                        , expect = Http.expectJson (FinishLoadingPage "SEARCH") decodeItems
                        }
            in
            ( { model | board = startLoadingNextPage "SEARCH" model.board }, request )

        SearchSimilar item ->
            let
                request =
                    Http.get
                        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?relatedToVideoId=" ++ item.youtubeId ++ "&type=video"
                        , expect = Http.expectJson (FinishLoadingItems "SIMILAR") decodeItems
                        }
            in
            ( { model | sidebarState = Similar, board = startLoading "SIMILAR" model.board, similarItem = Just item }, request )

        LoadMoreSimilar nextPage ->
            case model.similarItem of
                Just item ->
                    let
                        request =
                            Http.get
                                { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?relatedToVideoId=" ++ item.youtubeId ++ "&type=video&pageToken=" ++ nextPage
                                , expect = Http.expectJson (FinishLoadingPage "SIMILAR") decodeItems
                                }
                    in
                    ( { model | board = startLoadingNextPage "SIMILAR" model.board }, request )

                Nothing ->
                    model |> noComand

        UpdateTimeline timeline ->
            model.videoBeingPlayed
                |> Maybe.map (\videoPlayed -> { model | board = updateTimeline videoPlayed timeline model.board } |> markSelectedBoardAsNeededToSync)
                |> Maybe.withDefault model
                |> noComand

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
            { model | board = List.foldl mergeAndNormalizeResponse model.board boards } |> noComand

        CreateNewBoard ->
            ( model, createBoard () )

        OnBoardCreated boardResponse ->
            let
                profile =
                    model.userProfile

                newProfile =
                    { profile | selectedBoard = boardResponse.id, boards = profile.boards ++ [ boardResponse.id ] }
            in
            { model | board = mergeAndNormalizeResponse boardResponse model.board }
                |> updateProfileAndMarkAsNeededToSync newProfile
                |> markSelectedBoardAsNeededToSync
                |> noComand

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
            Set.toList model.boardIdsToSync |> List.map (\boardId -> denormalizeBoard boardId model.board) |> unpackMaybes

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
        (Json.field "items" (Json.list decodeItem))
        (Json.field "nextPageToken" Json.string)


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
                            |> List.map (\stack -> viewStack model [ class "column-board" ] stack.id)
                        )
                        [ button [ class "add-stack-button", onClick CreateSingleId ] [ text "Add column" ], div [ class "post-add-stack-space" ] [] ]
                    )
                ]

        Nothing ->
            div [] [ text "Loading BOARD" ]


viewBoardBar : Board -> Html Msg
viewBoardBar { name } =
    div [ class "board-title" ] [ text name ]


viewStack : Model -> List (Attribute Msg) -> String -> Html Msg
viewStack { renamingState, dragState, videoBeingPlayed, board } attributes stackId =
    case getStackToView board stackId of
        Just ( { id, name }, items ) ->
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

        Nothing ->
            div [] []


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
    , viewStackFooter LoadMoreSearch "SEARCH" model.board
    ]


viewStackFooter msg stackId model =
    case getStackStatus stackId model of
        Just (Ready (Just nextPage)) ->
            div [ class "sidebar-bottom-action-container" ] [ button [ onClick (msg nextPage), class "dark" ] [ text "load more" ] ]

        Just IsLoading ->
            progress [ class "material-progress-linear top" ] []

        Just IsLoadingNextPage ->
            progress [ class "material-progress-linear" ] []

        _ ->
            div [] []


viewSimilar model =
    let
        items =
            getItemsForStack "SIMILAR" model.board
    in
    [ viewSidebarHeader "Similar"
    , div [] (List.map (\item -> viewItem [] model.dragState model.videoBeingPlayed item) items)
    , viewStackFooter LoadMoreSimilar "SIMILAR" model.board
    ]


viewSidebarHeader title =
    div [ class "sidebar-header" ] [ h3 [] [ text title ], button [ onClick (SetSidebar Hidden), class "icon-button hide-icon" ] [ img [ src "/icons/chevron.svg" ] [] ] ]


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
        [ div [ class "item-image-container" ]
            [ img [ draggable "false", class "item-image", src ("https://i.ytimg.com/vi/" ++ item.youtubeId ++ "/mqdefault.jpg") ] []
            , viewItemDuration item
            ]
        , span [ class "item-text" ] [ text item.name ]
        , viewItemProgress item
        , div [ class "item-click-overlay", onMouseEnter (ItemEnterDuringDrag item.id) ]
            [ div [ class "item-drag-mouse-down-area", onMouseDown (ItemMouseDown item.id) ] []
            , div [ class "item-icon-area", onClick (SearchSimilar item) ] [ img [ draggable "false", src "/icons/similar.svg" ] [] ]
            ]
        ]


viewItemDuration item =
    case item.duration of
        Just duration ->
            div [ class "item-duration" ] [ text (formatTime duration) ]

        Nothing ->
            div [] []


viewItemProgress : Item -> Html msg
viewItemProgress item =
    case ( item.duration, item.currentTime ) of
        ( Just duration, Just currentTime ) ->
            let
                percent =
                    String.fromInt (round ((currentTime / duration) * 100)) ++ "%"
            in
            div [ class "item-progress-filled", style "width" percent ] []

        _ ->
            div [] []


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
            viewStack { model | renamingState = NoRename, dragState = DragState.init } (getAttributes elementPosition) stackId

        Just ( BoardBeingDragged, elementPosition, boardId ) ->
            model.board.boards
                |> Dict.get boardId
                |> Maybe.map (viewBoardButton DragState.init model (getAttributes elementPosition))
                |> Maybe.withDefault (div [] [])

        Nothing ->
            div [] []
