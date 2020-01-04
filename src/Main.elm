module Main exposing (main)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List.Extra exposing (findIndex, splitAt)


main : Program (Maybe ()) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Lean Watch", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        newModel =
            update msg model
    in
    ( newModel
    , Cmd.none
    )



-- MODEL


type alias Model =
    { stacks : Dict String (List String)
    , stacksOrder : List String
    , dragState : DragState
    }


type DragState
    = NoDrag
    | DraggingItem MouseMoveEvent Offsets String
    | DraggingStack MouseMoveEvent Offsets String


type Msg
    = Noop
    | StackTitleMouseDown String MouseDownEvent
    | ItemMouseDown String MouseDownEvent
    | MouseMove MouseMoveEvent
    | MouseUp
    | StackOverlayEnterDuringDrag String
    | StackEnterDuringDrag String
    | ItemEnterDuringDrag String


init : Maybe () -> ( Model, Cmd Msg )
init _ =
    ( { stacks =
            Dict.fromList
                [ ( "1", createItems 1 7 )
                , ( "2", createItems 8 12 )
                , ( "42", createItems 42 44 )
                , ( "Empty", [] )
                ]
      , stacksOrder = [ "1", "42", "2", "Empty" ]
      , dragState = NoDrag
      }
    , Cmd.none
    )


createItems : Int -> Int -> List String
createItems from to =
    List.map (\n -> "Item " ++ String.fromInt n) (List.range from to)


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



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemMouseDown itemId { mousePosition, offsets } ->
            { model | dragState = DraggingItem mousePosition offsets itemId }

        MouseUp ->
            { model | dragState = NoDrag }

        StackTitleMouseDown stackId { mousePosition, offsets } ->
            { model | dragState = DraggingStack mousePosition offsets stackId }

        MouseMove newMousePosition ->
            case ( model.dragState, newMousePosition.buttons ) of
                ( DraggingStack _ offsets id, 1 ) ->
                    { model | dragState = DraggingStack newMousePosition offsets id }

                ( DraggingItem _ offsets id, 1 ) ->
                    { model | dragState = DraggingItem newMousePosition offsets id }

                -- Any registered mouse move without mouse pressed is ending eny drag session
                ( _, 0 ) ->
                    { model | dragState = NoDrag }

                _ ->
                    model

        ItemEnterDuringDrag itemUnder ->
            case model.dragState of
                DraggingItem _ _ itemOver ->
                    if itemOver == itemUnder then
                        model

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
                        { model | stacks = newStacks }

                _ ->
                    model

        StackEnterDuringDrag stackUnder ->
            case model.dragState of
                DraggingStack _ _ stackOver ->
                    { model | stacksOrder = moveStackToAnotherPosition model stackOver stackUnder }

                _ ->
                    model

        StackOverlayEnterDuringDrag stackUnder ->
            case model.dragState of
                DraggingStack _ _ stackOver ->
                    { model | stacksOrder = moveStackToAnotherPosition model stackOver stackUnder }

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
                                model

                            else
                                { model
                                    | stacks =
                                        model.stacks
                                            |> updateStack fromStackId (removeItem itemOver)
                                            |> updateStack toStackId (\items -> List.append items [ itemOver ])
                                }

                        Nothing ->
                            { model
                                | stacks =
                                    model.stacks
                                        |> updateStack fromStackId (removeItem itemOver)
                                        |> updateStack toStackId (\_ -> [ itemOver ])
                            }

                _ ->
                    model

        Noop ->
            model


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
    div (List.append [ class ("board" ++ classIf (isDraggingAnything model.dragState) "board-during-drag"), onMouseUp MouseUp ] (getOnMouseMoveEventHandler model))
        (List.append
            (model.stacksOrder
                |> List.map (\stackId -> ( stackId, Dict.get stackId model.stacks |> Maybe.withDefault [] ))
                |> List.map (\( stackId, stackM ) -> viewStack model.dragState [class "column-board"] ( stackId, stackM ))
            )
            [ viewElementBeingDragged model ]
        )


getOnMouseMoveEventHandler model =
    case model.dragState of
        NoDrag ->
            []

        _ ->
            [ onMouseMove MouseMove ]


viewStack : DragState -> List (Attribute Msg) -> ( String, List String ) -> Html Msg
viewStack dragState attributes ( stackId, items ) =
    div (List.append [ class "column-drag-overlay" ] attributes)
        [ div
            [ class ("column" ++ classIf (isDraggingStack dragState stackId) "column-preview")
            , onMouseEnter (StackEnterDuringDrag stackId)
            ]
            [ div [ class "column-title", onMouseDown (StackTitleMouseDown stackId) ]
                [ span [] [ text ("Stack " ++ stackId) ]
                ]
            , div [class "column-content"]
                (if List.isEmpty items then
                    [ div [ class "empty-stack-placeholder", onMouseEnter (StackOverlayEnterDuringDrag stackId) ] [] ]

                 else
                    List.map (\item -> viewItem (isDraggingItem dragState item) item) items
                )
            ]
        , div [ class "column-footer", onMouseEnter (StackOverlayEnterDuringDrag stackId) ] []
        ]


viewItem : Bool -> String -> Html Msg
viewItem isDragging item =
    div
        [ class
            ("item"
                ++ classIf isDragging "item-preview"
            )
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
            div [] [ text "no item is being dragged" ]



-- CSS HELPERS


classIf : Bool -> String -> String
classIf condition class =
    if condition then
        " " ++ class

    else
        ""



-- HTML EVENTS DECODERS


type alias MouseMoveEvent =
    { pageX : Int
    , pageY : Int
    , buttons : Int
    }


type alias Offsets =
    { offsetX : Int
    , offsetY : Int
    }


type alias MouseDownEvent =
    { offsets : Offsets
    , mousePosition : MouseMoveEvent
    }


onMouseMove : (MouseMoveEvent -> msg) -> Attribute msg
onMouseMove tagger =
    on "mousemove" (Decode.map tagger mouseMoveDecoder)


onMouseDown : (MouseDownEvent -> msg) -> Attribute msg
onMouseDown tagger =
    on "mousedown" (Decode.map tagger mouseDownDecoder)


onMouseUp : msg -> Attribute msg
onMouseUp tagger =
    on "mouseup" (Decode.succeed tagger)


onMouseEnter : msg -> Attribute msg
onMouseEnter tagger =
    on "mouseenter" (Decode.succeed tagger)


mouseDownDecoder : Decode.Decoder MouseDownEvent
mouseDownDecoder =
    Decode.map2 MouseDownEvent offsetsDecoder mouseMoveDecoder


mouseMoveDecoder : Decode.Decoder MouseMoveEvent
mouseMoveDecoder =
    Decode.map3 MouseMoveEvent
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)
        (Decode.field "buttons" Decode.int)


offsetsDecoder : Decode.Decoder Offsets
offsetsDecoder =
    Decode.map2 Offsets
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)
