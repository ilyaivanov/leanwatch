port module Ports exposing (..)


port scrollItemToBeginning : ScrollOptions -> Cmd msg


type alias ScrollOptions =
    { top : Maybe Int
    , left : Maybe Int
    , elementId : String
    }


scrollToTop element =
    scrollItemToBeginning { elementId = element, left = Nothing, top = Just 0 }


scrollToLeft element =
    scrollItemToBeginning { elementId = element, left = Just 0, top = Nothing }
