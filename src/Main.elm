port module Main exposing (main)

import Browser
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List.Extra exposing (findIndex, splitAt)


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



-- MODEL


type alias Model =
    { items : List String
    , itemBeingDragged : Maybe String
    }


type alias DragOver =
    { offsetY : Int
    , offsetHeight : Int
    }


type Msg
    = OnDragOver String
    | DragStart String
    | DragEnd
    | Noop


init : Maybe Model -> ( Model, Cmd Msg )
init _ =
    ( { items = List.map (\n -> "Item " ++ String.fromInt n) (List.range 1 10)
      , itemBeingDragged = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnDragOver itemUnder ->
            case model.itemBeingDragged of
                Nothing ->
                    model

                Just itemOver ->
                    let
                        index =
                            findIndex (equals itemUnder) model.items |> Maybe.withDefault -1

                        items =
                            model.items
                                |> List.filter (notEquals itemOver)
                                |> insertInto index itemOver
                    in
                    { model | items = items }

        DragStart item ->
            { model | itemBeingDragged = Just item }

        DragEnd ->
            { model | itemBeingDragged = Nothing }

        Noop ->
            model


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


equals : val -> val -> Bool
equals a b =
    a == b


notEquals : val -> val -> Bool
notEquals a b =
    a /= b



-- VIEW


view : Model -> Html Msg
view model =
    let
        dragId =
            model.itemBeingDragged |> Maybe.withDefault ""
    in
    div [ onDragOver Noop, class "foo" ] (List.map (\item -> viewItem (item == dragId) item) model.items)


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
    preventDefaultOn "dragover" (Decode.succeed (alwaysPreventDefault msg))


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
