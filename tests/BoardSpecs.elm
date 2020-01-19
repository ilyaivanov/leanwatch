module BoardSpecs exposing (..)

import Dict exposing (Dict)
import DictMoves exposing (moveItem, moveItemToEndOfStack)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    concat
        [ test "Moving an item from one stack to another stack"
            (\_ ->
                let
                    initial =
                        Dict.fromList
                            [ ( "Stack 1", { id = "Stack 1", items = [ "Item_1", "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", items = [ "Item_3", "Item_4" ] } )
                            ]

                    expected =
                        Dict.fromList
                            [ ( "Stack 1", { id = "Stack 1", items = [ "Item_1", "Item_3", "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", items = [ "Item_4" ] } )
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
                            [ ( "Stack 1", { id = "Stack 1", items = [ "Item_1", "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", items = [ "Item_3", "Item_4" ] } )
                            ]

                    expected =
                        Dict.fromList
                            [ ( "Stack 1", { id = "Stack 1", items = [ "Item_2" ] } )
                            , ( "Stack 2", { id = "Stack 2", items = [ "Item_3", "Item_4", "Item_1" ] } )
                            ]

                    received =
                        moveItemToEndOfStack initial { itemToMove = "Item_1", targetStack = "Stack 2" }
                in
                received |> Expect.equal expected
            )
        ]
