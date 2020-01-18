port module Board exposing (..)

import Dict exposing (Dict)
import Embed.Youtube
import Embed.Youtube.Attributes
import ExtraEvents exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Json
import List.Extra exposing (findIndex, splitAt)
import Login
import Process
import Random
import Task


noComand : Model -> ( Model, Cmd msg )
noComand model =
    ( model, Cmd.none )


port loadBoard : String -> Cmd msg


port onUserProfileLoaded : (UserProfile -> msg) -> Sub msg


port onBoardLoaded : (BoardResponse -> msg) -> Sub msg


port saveBoard : BoardResponse -> Cmd msg



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
            Dict.fromList (List.map (\s -> ( s.id, Stack s.id s.name (getIds s.items) )) boardResponse.stacks)

        allItems =
            boardResponse.stacks |> List.map (\s -> s.items) |> List.concat

        items =
            Dict.fromList (List.map (\i -> ( i.id, { id = i.id, name = i.name, youtubeId = i.youtubeId } )) allItems)

        getIds =
            List.map (\s -> s.id)
    in
    { model
        | boards = Dict.insert boardResponse.id (Board boardResponse.id boardResponse.name (getIds boardResponse.stacks)) model.boards
        , stacks = Dict.union stacks model.stacks
        , items = Dict.union items model.items
    }


denormalizeBoard : Board -> Model -> BoardResponse
denormalizeBoard board model =
    let
        stacksNarrow =
            board.stacks |> List.map (\id -> Dict.get id model.stacks) |> unpackMaybes

        stacks =
            stacksNarrow |> List.map (\s -> { id = s.id, name = s.name, items = s.items |> List.map (\id -> Dict.get id model.items) |> unpackMaybes })
    in
    { id = board.id
    , name = board.name
    , stacks = stacks
    }



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
    , searchTerm : String

    -- Used to debounce search on input
    , currentSearchId : String
    }


type DragState
    = NoDrag
    | ItemPressedNotYetMoved MouseMoveEvent Offsets String
    | DraggingItem MouseMoveEvent Offsets String
    | DraggingStack MouseMoveEvent Offsets String


type SidebarState
    = Hidden
    | Search
    | Boards


type alias Board =
    { id : String
    , name : String
    , stacks : List String
    }


type Msg
    = Noop
      -- DND events
    | StackTitleMouseDown String MouseDownEvent
    | ItemMouseDown String MouseDownEvent
    | MouseMove MouseMoveEvent
    | MouseUp
    | StackOverlayEnterDuringDrag String
    | StackEnterDuringDrag String
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
    | HideSidebar
    | ShowSearch
    | ShowBoards
      -- Board Management
    | SelectBoard String
    | UserProfileLoaded UserProfile
    | BoardLoaded BoardResponse
    | SaveSelectedBoard


init : Model
init =
    { stacks = Dict.empty
    , items = Dict.empty
    , boards = Dict.empty
    , userProfile =
        { selectedBoard = ""
        , boards = []
        }
    , dragState = NoDrag
    , searchTerm = ""
    , currentSearchId = ""
    , videoBeingPlayed = Nothing
    , sidebarState = Boards
    }


type alias Stack =
    { id : String
    , name : String
    , items : List String
    }


type alias UserProfile =
    { boards : List BoardInfo
    , selectedBoard : String
    }


type alias BoardInfo =
    { id : String, name : String }


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


shouldListenToMoveEvents : DragState -> Bool
shouldListenToMoveEvents dragState =
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


getStackByItem : String -> Dict String Stack -> Stack
getStackByItem item stacks =
    Dict.toList stacks
        |> List.filter (\( _, stack ) -> List.member item stack.items)
        |> List.map Tuple.second
        |> List.head
        |> Maybe.withDefault (Stack "NOT_FOUND" "NOT_FOUND" [])


getItemById : String -> Model -> Maybe Item
getItemById itemId model =
    Dict.toList model.items
        |> List.map Tuple.second
        |> List.filter (\i -> i.id == itemId)
        |> List.head


getStack : String -> Dict String Stack -> Stack
getStack stackId stacks =
    Dict.get stackId stacks |> Maybe.withDefault (Stack "NOT_FOUND" "NOT_FOUND" [])


isBoards : SidebarState -> Bool
isBoards state =
    case state of
        Boards ->
            True

        _ ->
            False


isSearch : SidebarState -> Bool
isSearch state =
    case state of
        Search ->
            True

        _ ->
            False


isHidden : SidebarState -> Bool
isHidden state =
    case state of
        Hidden ->
            True

        _ ->
            False


getBoardViewModel : Model -> Maybe Board
getBoardViewModel model =
    Dict.get model.userProfile.selectedBoard model.boards



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ItemMouseDown itemId { mousePosition, offsets } ->
            noComand { model | dragState = ItemPressedNotYetMoved mousePosition offsets itemId }

        MouseUp ->
            case model.dragState of
                ItemPressedNotYetMoved _ _ id ->
                    noComand { model | dragState = NoDrag, videoBeingPlayed = Just id }

                _ ->
                    noComand { model | dragState = NoDrag }

        StackTitleMouseDown stackId { mousePosition, offsets } ->
            noComand { model | dragState = DraggingStack mousePosition offsets stackId }

        MouseMove newMousePosition ->
            case ( model.dragState, newMousePosition.buttons ) of
                ( DraggingStack _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingStack newMousePosition offsets id }

                ( ItemPressedNotYetMoved _ offsets id, 1 ) ->
                    noComand { model | dragState = DraggingItem newMousePosition offsets id }

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
                    noComand { model | boards = moveStackToAnotherPosition model stackOver stackUnder }

                _ ->
                    noComand model

        StackOverlayEnterDuringDrag stackUnder ->
            case model.dragState of
                DraggingStack _ _ stackOver ->
                    noComand { model | boards = moveStackToAnotherPosition model stackOver stackUnder }

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
                    | boards = updateBoard model.userProfile.selectedBoard (\b -> { b | stacks = List.append b.stacks [ newStackId ] }) model.boards
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

        GotItems response ->
            case response of
                Ok body ->
                    let
                        newItems =
                            body.items

                        ids =
                            List.map (\i -> i.id) newItems

                        newItemsDict =
                            Dict.fromList (List.map (\i -> ( i.id, i )) newItems)

                        itemsUpdated =
                            Dict.union newItemsDict model.items
                    in
                    noComand { model | stacks = Dict.update "SEARCH" (\_ -> Just (Stack "SEARCH" "New Stack" ids)) model.stacks, items = itemsUpdated }

                _ ->
                    noComand model

        ShowBoards ->
            noComand { model | sidebarState = Boards }

        ShowSearch ->
            noComand { model | sidebarState = Search }

        HideSidebar ->
            noComand { model | sidebarState = Hidden }

        SelectBoard boardId ->
            if boardId == model.userProfile.selectedBoard then
                ( model, Cmd.none )

            else
                let
                    profile =
                        model.userProfile

                    selectedBoard =
                        Dict.get boardId model.boards

                    action =
                        case selectedBoard of
                            Just _ ->
                                Cmd.none

                            Nothing ->
                                loadBoard boardId
                in
                ( { model | userProfile = { profile | selectedBoard = boardId } }, action )

        UserProfileLoaded profile ->
            ( { model | userProfile = profile }, loadBoard profile.selectedBoard )

        BoardLoaded board ->
            noComand (mergeAndNormalizeResponse board model)

        SaveSelectedBoard ->
            case Dict.get model.userProfile.selectedBoard model.boards of
                Just actualBoard ->
                    ( model, saveBoard (denormalizeBoard actualBoard model) )

                Nothing ->
                    noComand model

        Noop ->
            noComand model


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


createIds =
    Random.list 10 createId


createId =
    Random.float 0 1 |> Random.map String.fromFloat


unpackMaybes : List (Maybe item) -> List item
unpackMaybes maybes =
    List.filterMap identity maybes


moveStackToAnotherPosition : Model -> String -> String -> Dict String Board
moveStackToAnotherPosition model stackOver stackUnder =
    let
        board =
            Dict.get model.userProfile.selectedBoard model.boards |> Maybe.withDefault { id = "", name = "", stacks = [] }

        targetIndex =
            findIndex (equals stackUnder) board.stacks |> Maybe.withDefault -1
    in
    model.boards
        |> updateBoard model.userProfile.selectedBoard (\s -> { s | stacks = removeItem stackOver s.stacks })
        |> updateBoard model.userProfile.selectedBoard (\s -> { s | stacks = insertInto targetIndex stackOver s.stacks })


updateStack : String -> (Stack -> Stack) -> Dict String Stack -> Dict String Stack
updateStack stackId updater stacks =
    Dict.update stackId (Maybe.map (\v -> updater v)) stacks


updateBoard : String -> (Board -> Board) -> Dict String Board -> Dict String Board
updateBoard boardId updater boards =
    Dict.update boardId (Maybe.map (\v -> updater v)) boards


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


view : Model -> Maybe Login.LoginSuccessResponse -> Html Msg
view model login =
    case model.userProfile.selectedBoard of
        -- ugly assumption, but works for now. Consider using a separate precise state of loading
        -- maybe even load your state progressively
        "" ->
            div [] [ text "Loading user profile..." ]

        _ ->
            div (attributesIf (shouldListenToMoveEvents model.dragState) [ onMouseMove MouseMove, onMouseUp MouseUp ])
                [ viewTopBar model login
                , div []
                    [ viewSidebar model
                    , viewBoard model
                    ]
                , viewPlayer model
                ]


viewTopBar : Model -> Maybe Login.LoginSuccessResponse -> Html Msg
viewTopBar model login =
    div [ class "top-bar" ]
        [ div []
            [ button [ classIf (isBoards model.sidebarState) "active", onClick ShowBoards ] [ text "boards" ]
            , button [ classIf (isSearch model.sidebarState) "active", onClick ShowSearch ] [ text "search" ]
            ]
        , Maybe.map viewUser login |> Maybe.withDefault (div [] [])
        ]


viewUser : Login.LoginSuccessResponse -> Html Msg
viewUser loginInfo =
    div [ class "user-info-container" ]
        [ button [ onClick SaveSelectedBoard ] [ text "save" ]
        , span [] [ text loginInfo.displayName ]
        , img [ class "user-info-image", src loginInfo.photoURL ] []
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    case getBoardViewModel model of
        Just board ->
            div
                [ class "board"
                , classIf (not (isHidden model.sidebarState)) "board-with-sidebar"
                , classIf (isDraggingAnything model.dragState) "board-during-drag"
                ]
                [ viewBoardBar board
                , div
                    [ class "columns-container" ]
                    (List.append
                        (board.stacks
                            |> List.map (\stackId -> Dict.get stackId model.stacks)
                            |> unpackMaybes
                            |> List.map (\stack -> viewStack model.dragState [ class "column-board" ] (getStackToView model stack.id))
                        )
                        [ button [ class "add-stack-button", onClick CreateSingleId ] [ text "add" ], viewElementBeingDragged model ]
                    )
                ]

        Nothing ->
            div [] [ text "Loading BOARD" ]


viewBoardBar : Board -> Html Msg
viewBoardBar { name } =
    div [ class "board-header" ] [ text name ]


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
                    stack.items |> List.map (\itemId -> Dict.get itemId model.items) |> unpackMaybes

                Nothing ->
                    []
    in
    [ div [ class "sidebar-header" ] [ h3 [] [ text "Search" ], button [ onClick HideSidebar ] [ text "<" ] ]
    , input [ onInput OnSearchInput, placeholder "Find videos by name...", value model.searchTerm ] []
    , div [] (List.map (\item -> viewItem [] (isDraggingItem model.dragState item) item) items)
    ]


viewBoards model =
    [ div [ class "sidebar-header sidebar-padded" ]
        [ h3 [] [ text "Boards" ]
        , button [ onClick HideSidebar ] [ text "<" ]
        ]
    , div [] (model.userProfile.boards |> List.map (viewBoardButton model))
    ]


viewBoardButton model { name, id } =
    div [ class "sidebar-boards-button", classIf (model.userProfile.selectedBoard == id) "active", onClick (SelectBoard id) ] [ text name ]


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
            viewStack NoDrag
                [ class "item-dragged"
                , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                ]
                ( stack, items )

        _ ->
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
