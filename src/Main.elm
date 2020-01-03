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
    , itemBeingDragged : Maybe String
    }


type Msg
    = OnDragOver String
    | DragStart String
    | DragEnd
    | Noop


init : Maybe () -> ( Model, Cmd Msg )
init _ =
    ( { stacks =
            Dict.fromList
                [ ( "1", createItems 1 7 )
                , ( "2", createItems 8 12 )
                ]
      , itemBeingDragged = Nothing
      }
    , Cmd.none
    )


createItems : Int -> Int -> List String
createItems from to =
    List.map (\n -> "Item " ++ String.fromInt n) (List.range from to)



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnDragOver itemUnder ->
            case model.itemBeingDragged of
                Nothing ->
                    model

                Just itemOver ->
                    if itemOver == itemUnder then
                        model

                    else
                        let
                            getStackByItem item =
                                Dict.toList model.stacks
                                    |> List.filter (\( _, value ) -> List.member item value)
                                    |> List.head
                                    |> Maybe.withDefault ( "NOT_FOUND", [] )

                            updateStack stackId updater stacks =
                                Dict.update stackId (Maybe.map (\v -> updater v)) stacks

                            removeItem item items =
                                List.filter (notEquals item) items

                            ( fromStackId, _ ) =
                                getStackByItem itemOver

                            ( toStackId, toStackItems ) =
                                getStackByItem itemUnder

                            targetIndex =
                                findIndex (equals itemUnder) toStackItems |> Maybe.withDefault -1

                            newStacks =
                                model.stacks
                                    |> updateStack fromStackId (removeItem itemOver)
                                    |> updateStack toStackId (insertInto targetIndex itemOver)
                        in
                        { model | stacks = newStacks }

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
    div [ onDragOver Noop, class "board" ]
        [ viewStack "Stack 1" dragId (Dict.get "1" model.stacks |> Maybe.withDefault [])
        , viewStack "Stack 2" dragId (Dict.get "2" model.stacks |> Maybe.withDefault [])
        ]


viewStack : String -> String -> List String -> Html Msg
viewStack stackName dragId items =
    div [ class "column" ]
        [ h2 [] [ text stackName ]
        , div [] (List.map (\item -> viewItem (item == dragId) item) items)
        ]


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
