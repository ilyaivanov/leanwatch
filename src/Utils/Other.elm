module Utils.Other exposing (..)

import List.Extra as LE exposing (..)


flip f a b =
    f b a


removeItem itemToRemove list =
    List.filter ((/=) itemToRemove) list


unpackMaybes : List (Maybe item) -> List item
unpackMaybes maybes =
    List.filterMap identity maybes


getNextItem : a -> List a -> Maybe a
getNextItem item list =
    list
        |> LE.elemIndex item
        |> Maybe.map ((+) 1)
        |> Maybe.andThen (\i -> LE.getAt i list)


ifNothing maybeToCheck otherMaybe =
    case maybeToCheck of
        Just a ->
            Just a

        Nothing ->
            otherMaybe


maybeHasValue maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


formatTime time =
    let
        timeInt =
            floor time

        seconds =
            modBy 60 timeInt

        minutes =
            modBy 60 (timeInt // 60)

        hours =
            round time // (60 * 60)

        hoursPrefix =
            if hours > 0 then
                String.fromInt hours ++ ":"

            else
                ""

        minutesFormatted =
            if minutes < 10 then
                "0" ++ String.fromInt minutes

            else
                String.fromInt minutes
    in
    hoursPrefix ++ minutesFormatted ++ ":" ++ String.fromInt seconds
