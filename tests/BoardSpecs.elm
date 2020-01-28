module BoardSpecs exposing (..)

import Dict exposing (Dict)
import DictMoves exposing (moveItem, moveItemToEnd)
import Expect exposing (Expectation)
import Test exposing (..)
import Utils.Other exposing (formatTime)


suite : Test
suite =
    concat
        [ test "Moving an item from one stack to another stack"
            (\_ ->
                let
                    initial =
                        Dict.fromList
                            [ ( "Stack 1", { id = "Stack 1", children = [ "Item_1", "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", children = [ "Item_3", "Item_4" ] } )
                            ]

                    expected =
                        Dict.fromList
                            [ ( "Stack 1", { id = "Stack 1", children = [ "Item_1", "Item_3", "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", children = [ "Item_4" ] } )
                            ]

                    received =
                        moveItem initial { from = "Item_3", to = "Item_2" }
                in
                received |> Expect.equal expected
            )
        , test "Moving item from one stack to the end of another stack"
            (\_ ->
                let
                    initial =
                        Dict.fromList
                            [ ( "Stack 1", { id = "Stack 1", children = [ "Item_1", "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", children = [ "Item_3", "Item_4" ] } )
                            ]

                    expected =
                        Dict.fromList
                            [ ( "Stack 1", { id = "Stack 1", children = [ "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", children = [ "Item_3", "Item_4", "Item_1" ] } )
                            ]

                    received =
                        moveItemToEnd initial { itemToMove = "Item_1", targetParent = "Stack 2" }
                in
                received |> Expect.equal expected
            )
        , test "Formatting 1:32:45"
            (\_ ->
                formatTime (1 * 60 * 60 + 32 * 60 + 45 + 0.25) |> Expect.equal "1:32:45"
            )
        , test "Formatting 1:02:45"
            (\_ ->
                formatTime (1 * 60 * 60 + 2 * 60 + 45 + 0.25) |> Expect.equal "1:02:45"
            )
        , test "Formatting 2:45"
            (\_ ->
                formatTime (2 * 60 + 45 + 0.25) |> Expect.equal "2:45"
            )
        , test "Formatting 2:05"
            (\_ ->
                formatTime (2 * 60 + 5 + 0.25) |> Expect.equal "2:05"
            )
        ]
