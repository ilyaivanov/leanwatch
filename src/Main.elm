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
    | MouseOverStackDragHandler String
    | DraggingStack String
    | DraggingItem String


type Msg
    = OnDragOver String
    | DragStart String
    | DragEnd
    | EnableDragForStack String
    | DisableDragForStack
    | OnStackDragOver String
    | OnStackDragStart
    | OnStackDragEnd
    | Noop


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
        DraggingStack stackBeingDragged ->
            stackBeingDragged == stackId

        _ ->
            False


isDraggingItem : DragState -> String -> Bool
isDraggingItem dragState itemId =
    case dragState of
        DraggingItem itemBeingDragged ->
            itemBeingDragged == itemId

        _ ->
            False


isDraggingAnyItem : DragState -> Bool
isDraggingAnyItem dragState =
    case dragState of
        DraggingItem _ ->
            True

        _ ->
            False


isStackReadyForDrag : DragState -> String -> Bool
isStackReadyForDrag dragState stackId =
    case dragState of
        MouseOverStackDragHandler stackOverMouse ->
            stackOverMouse == stackId

        _ ->
            False



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        DragStart item ->
            { model | dragState = DraggingItem item }

        DragEnd ->
            { model | dragState = NoDrag }

        EnableDragForStack stackId ->
            --For some reason onMouseEnter handler triggers during drag, ignore those events
            -- if model.isStackDragging then
            case model.dragState of
                DraggingStack _ ->
                    model

                _ ->
                    { model | dragState = MouseOverStackDragHandler (log "EnableDragForStack" stackId) }

        DisableDragForStack ->
            --For some reason onMouseLeave handler triggers during drag, ignore those events
            case model.dragState of
                DraggingStack _ ->
                    model

                _ ->
                    { model | dragState = NoDrag }

        OnStackDragEnd ->
            { model | dragState = log "OnStackDragEnd" NoDrag }

        OnDragOver itemUnder ->
            case model.dragState of
                DraggingItem itemOver ->
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

        OnStackDragOver stackUnder ->
            case model.dragState of
                DraggingStack stackOver ->
                    let
                        targetIndex =
                            findIndex (equals stackUnder) model.stacksOrder |> Maybe.withDefault -1

                        stacksOrder =
                            model.stacksOrder
                                |> removeItem stackOver
                                |> insertInto targetIndex stackOver
                    in
                    { model | stacksOrder = stacksOrder }

                DraggingItem itemOver ->
                    let
                        ( fromStackId, fromItems ) =
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

        OnStackDragStart ->
            case model.dragState of
                MouseOverStackDragHandler stackId ->
                    { model | dragState = DraggingStack stackId }

                _ ->
                    model

        Noop ->
            model


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
    div [ onDragOver Noop, onMouseUp OnStackDragEnd, class "board" ]
        (model.stacksOrder
            |> List.map (\stackId -> ( stackId, Dict.get stackId model.stacks |> Maybe.withDefault [] ))
            |> List.map (\( stackId, stackM ) -> viewStack model.dragState ( stackId, stackM ))
        )


viewStack : DragState -> ( String, List String ) -> Html Msg
viewStack dragState ( stackId, items ) =
    div [ class "column-drag-overlay", onDragOver (OnStackDragOver stackId) ]
        [ div
            [ class ("column" ++ classIf (isDraggingStack dragState stackId) "column-preview")
            , draggable (boolToString (isStackReadyForDrag dragState stackId))
            , onDragStart OnStackDragStart
            , onDragEnd OnStackDragEnd
            , onDragOverCustom Noop (isDraggingAnyItem dragState)
            ]
            [ div [ class "column-title", onMouseLeave DisableDragForStack, onMouseEnter (EnableDragForStack stackId) ]
                [ span [] [ text ("Stack " ++ stackId) ]
                ]
            , div []
                (if List.isEmpty items then
                    [ div [ class "empty-stack-placeholder", onDragOver (OnStackDragOver stackId) ] [] ]

                 else
                    List.map (\item -> viewItem (isDraggingItem dragState item) item) items
                )
            ]
        ]


boolToString : Bool -> String
boolToString cond =
    if cond then
        "true"

    else
        "false"


viewItem : Bool -> String -> Html Msg
viewItem isDragging item =
    div
        [ class
            ("item"
                ++ classIf isDragging "item-preview"
            )
        , onDragOver (OnDragOver item)
        , onDragStart (DragStart item)
        , onDragEnd DragEnd
        , draggable "true"
        ]
        [ text item ]



-- CSS HELPERS


classIf : Bool -> String -> String
classIf condition class =
    if condition then
        " " ++ class

    else
        ""



-- HTML EVENTS DECODERS


onDragOver : msg -> Attribute msg
onDragOver msg =
    custom "dragover" (Decode.succeed { message = msg, stopPropagation = True, preventDefault = True })


onDragOverCustom : msg -> Bool -> Attribute msg
onDragOverCustom msg stopPropagation =
    custom "dragover" (Decode.succeed { message = msg, stopPropagation = stopPropagation, preventDefault = True })


onDragStart : msg -> Attribute msg
onDragStart msg =
    on "dragstart" (Decode.succeed msg)


onDragEnd : msg -> Attribute msg
onDragEnd msg =
    preventDefaultOn "dragend" (Decode.succeed (alwaysPreventDefault msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )



-- this might be usefull if I decide to implement swapping strategy based on cursot offsetY and element height
-- current problem is that I don't know how to decode event and prevent default as well
-- pointDecoder : Decode.Decoder DragOver
-- pointDecoder =
--     Decode.map2 DragOver
--         (Decode.field "offsetY" Decode.int)
--         (Decode.at [ "target", "offsetHeight" ] Decode.int)
