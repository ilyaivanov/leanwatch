module ExtraEvents exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode


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


onClickIf : Bool -> msg -> Attribute msg
onClickIf condition msg =
    if condition then
        onClick msg

    else
        emptyAttribute


classIf : Bool -> String -> Attribute msg
classIf condition className =
    if condition then
        class className

    else
        emptyAttribute


attributesIf : Bool -> List (Attribute msg) -> List (Attribute msg)
attributesIf condition attributes =
    if condition then
        attributes

    else
        []


emptyAttribute =
    class ""
