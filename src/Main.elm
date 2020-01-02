port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Lean Watch", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        newModel =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, Cmd.none ]
    )


type alias Model =
    { items : List String
    , itemBeingDragged : Maybe String
    }


type alias DragOver =
    { offsetY : Int
    , offsetHeight : Int
    }


type Msg
    = OnDragOver String DragOver
    | DragStart String
    | DragEnd


pointDecoder : Decode.Decoder DragOver
pointDecoder =
    Decode.map2 DragOver
        (Decode.field "offsetY" Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)


onDragOver : (DragOver -> msg) -> Attribute msg
onDragOver tagger =
    on "dragover" (Decode.map tagger pointDecoder)


onDragStart : msg -> Attribute msg
onDragStart msg =
    on "dragstart" (Decode.succeed msg)


onDragEnd : msg -> Attribute msg
onDragEnd msg =
    on "dragend" (Decode.succeed msg)


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnDragOver item _ ->
            --dragging model.itemBeingDragged over item
            model

        DragStart item ->
            { model | itemBeingDragged = Just item }

        DragEnd ->
            { model | itemBeingDragged = Nothing }


init : Maybe Model -> ( Model, Cmd Msg )
init _ =
    ( { items = List.map (\n -> "Item " ++ String.fromInt n) (List.range 1 10)
      , itemBeingDragged = Nothing
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        dragId =
            model.itemBeingDragged |> Maybe.withDefault ""
    in
    div [] (List.map (\item -> viewItem (item == dragId) item) model.items)


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


classIf : Bool -> String -> String
classIf condition class =
    if condition then
        " " ++ class

    else
        ""
