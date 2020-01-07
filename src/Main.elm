module Main exposing (main)

import Browser
import Dict exposing (Dict)
import ExtraEvents exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (findIndex, splitAt)
import Random


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
    { stacks : Dict String (List String)
    , stacksOrder : List String
    , dragState : DragState
    , searchTerm : String
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
    | CreateStack Float
    | OnSearchInput String
      --Commands
    | CreateSingleId



--| CreateMultipleId Int Msg


init : Maybe () -> ( Model, Cmd Msg )
init _ =
    ( { stacks =
            Dict.fromList
                [ ( "1", createItems 1 7 )
                , ( "2", createItems 8 12 )
                , ( "42", createItems 42 44 )
                , ( "Empty", [] )
                , ( "SEARCH", createItems 15 25 )
                ]
      , stacksOrder = [ "1", "42", "2", "Empty" ]
      , dragState = NoDrag
      , searchTerm = ""
      }
    , Cmd.none
    )


createItems : Int -> Int -> List String
createItems =
    createItemsWithPostfix ""


createItemsWithPostfix : String -> Int -> Int -> List String
createItemsWithPostfix postfix from to =
    List.map (\n -> "Item " ++ String.fromInt n ++ postfix) (List.range from to)


isDraggingStack : DragState -> String -> Bool
isDraggingStack dragState stackId =
    case dragState of
        DraggingStack _ _ stackBeingDragged ->
            stackBeingDragged == stackId

        _ ->
            False


isDraggingItem : DragState -> String -> Bool
isDraggingItem dragState itemId =
    case dragState of
        DraggingItem _ _ itemBeingDragged ->
            itemBeingDragged == itemId

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


getSearchItems : Model -> List String
getSearchItems model =
    model.stacks |> Dict.get "SEARCH" |> Maybe.withDefault []



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
                            ( fromStackId, _ ) =
                                getStackByItem itemOver model.stacks

                            ( toStackId, toStackItems ) =
                                getStackByItem itemUnder model.stacks

                            targetIndex =
                                findIndex (equals itemUnder) toStackItems |> Maybe.withDefault -1

                            newStacks =
                                model.stacks
                                    |> updateStack fromStackId (removeItem itemOver)
                                    |> updateStack toStackId (insertInto targetIndex itemOver)
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
                        ( fromStackId, _ ) =
                            getStackByItem itemOver model.stacks

                        ( toStackId, toItems ) =
                            getStack stackUnder model.stacks
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
                                                |> updateStack fromStackId (removeItem itemOver)
                                                |> updateStack toStackId (\items -> List.append items [ itemOver ])
                                    }

                        Nothing ->
                            noComand
                                { model
                                    | stacks =
                                        model.stacks
                                            |> updateStack fromStackId (removeItem itemOver)
                                            |> updateStack toStackId (\_ -> [ itemOver ])
                                }

                _ ->
                    noComand model

        CreateStack id ->
            let
                newStackId =
                    String.fromFloat id
            in
            noComand
                { model
                    | stacksOrder = List.append model.stacksOrder [ newStackId ]
                    , stacks = Dict.insert newStackId [] model.stacks
                }

        OnSearchInput val ->
            noComand
                { model
                    | searchTerm = val
                    , stacks = Dict.update "SEARCH" (\_ -> Just (createItemsWithPostfix val 15 25)) model.stacks
                }

        CreateSingleId ->
            ( model, Random.generate CreateStack probability )

        Noop ->
            noComand model


probability =
    Random.float 0 1


moveStackToAnotherPosition model stackOver stackUnder =
    let
        targetIndex =
            findIndex (equals stackUnder) model.stacksOrder |> Maybe.withDefault -1
    in
    model.stacksOrder
        |> removeItem stackOver
        |> insertInto targetIndex stackOver


updateStack : String -> (List String -> List String) -> Dict String (List String) -> Dict String (List String)
updateStack stackId updater stacks =
    Dict.update stackId (Maybe.map (\v -> updater v)) stacks


getStackByItem : String -> Dict String (List String) -> ( String, List String )
getStackByItem item stacks =
    Dict.toList stacks
        |> List.filter (\( _, value ) -> List.member item value)
        |> List.head
        |> Maybe.withDefault ( "NOT_FOUND", [] )


getStack : String -> Dict String (List String) -> ( String, List String )
getStack stackId stacks =
    ( stackId
    , Dict.get stackId stacks |> Maybe.withDefault []
    )


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
                        |> List.map (\stackId -> ( stackId, Dict.get stackId model.stacks |> Maybe.withDefault [] ))
                        |> List.map (\( stackId, stackM ) -> viewStack model.dragState [ class "column-board" ] ( stackId, stackM ))
                    )
                    [ button [ class "add-stack-button", onClick CreateSingleId ] [ text "add" ], viewElementBeingDragged model ]
                )
            ]
        ]


viewStack : DragState -> List (Attribute Msg) -> ( String, List String ) -> Html Msg
viewStack dragState attributes ( stackId, items ) =
    div (List.append [ class "column-drag-overlay" ] attributes)
        [ div
            [ class "column"
            , classIf (isDraggingStack dragState stackId) "column-preview"
            , onMouseEnter (StackEnterDuringDrag stackId)
            ]
            [ div [ class "column-title", onMouseDown (StackTitleMouseDown stackId) ]
                [ span [] [ text ("Stack " ++ stackId) ]
                ]
            , div [ class "column-content" ]
                (if List.isEmpty items then
                    [ div [ class "empty-stack-placeholder", onMouseEnter (StackOverlayEnterDuringDrag stackId) ] [] ]

                 else
                    List.map (\item -> viewItem (isDraggingItem dragState item) item) items
                )
            ]
        , div [ class "column-footer", onMouseEnter (StackOverlayEnterDuringDrag stackId) ] []
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "sidebar" ]
        [ input [ onInput OnSearchInput, placeholder "Find videos by name...", value model.searchTerm ] []
        , div [] (List.map (\item -> viewItem (isDraggingItem model.dragState item) item) (getSearchItems model))
        ]


viewItem : Bool -> String -> Html Msg
viewItem isDragging item =
    div
        [ class "item"
        , classIf isDragging "item-preview"
        , onMouseDown (ItemMouseDown item)
        , onMouseEnter (ItemEnterDuringDrag item)
        ]
        [ text item ]


viewElementBeingDragged model =
    case model.dragState of
        DraggingItem mouseMoveEvent offsets itemId ->
            div
                [ class "item item-dragged"
                , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                ]
                [ text itemId ]

        DraggingStack mouseMoveEvent offsets stackId ->
            viewStack NoDrag
                [ class "item-dragged"
                , style "left" (String.fromInt (mouseMoveEvent.pageX - offsets.offsetX) ++ "px")
                , style "top" (String.fromInt (mouseMoveEvent.pageY - offsets.offsetY) ++ "px")
                ]
                (getStack stackId model.stacks)

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
