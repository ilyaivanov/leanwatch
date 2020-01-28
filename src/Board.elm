module Board exposing (Board, BoardModel, Item, Stack, init)

import Dict exposing (Dict)
import DictMoves exposing (Parent)
import Utils.Other exposing (unpackMaybes)


type alias Board =
    Parent
        { name : String
        }


type alias Stack =
    Parent
        { name : String
        }


type alias Item =
    { id : String
    , name : String
    , youtubeId : String
    }


type alias BoardModel =
    { boards : Dict String Board
    , stacks : Dict String Stack
    , items : Dict String Item
    }


init : BoardModel
init =
    { stacks = Dict.empty
    , items = Dict.empty
    , boards = Dict.empty
    }
