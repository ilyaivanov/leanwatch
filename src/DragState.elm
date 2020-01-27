module DragState exposing
    ( DragState
    , ItemBeingDragged(..)
    , getItemBeingDragged
    , handleBoardEnter
    , handleBoardMouseDown
    , handleItemEnter
    , handleItemMouseDown
    , handleMouseMove
    , handleMouseUp
    , handleStackEnter
    , handleStackOverlayEnter
    , handleStackTitleMouseDown
    , init
    , isDraggingAnyBoard
    , isDraggingAnything
    , isDraggingBoard
    , isDraggingItem
    , isDraggingStack
    , shouldListenToMouseMoveEvents
    )

import DictMoves exposing (moveItem, moveItemInList, moveItemToEnd)
import Utils.ExtraEvents exposing (MouseDownEvent, MouseMoveEvent, Offsets)


type DragState
    = NoDrag
    | ItemPressedNotYetMoved MouseMoveEvent Offsets String
    | BoardPressedNotYetMoved MouseMoveEvent Offsets String
    | DraggingItem MouseMoveEvent Offsets String
    | DraggingStack MouseMoveEvent Offsets String
    | DraggingBoard MouseMoveEvent Offsets String


type ItemBeingDragged
    = BoardBeingDragged
    | StackBeingDragged
    | ItemBeingDragged


getElementOffsets mousePosition offsets =
    { top = mousePosition.pageY - offsets.offsetY
    , left = mousePosition.pageX - offsets.offsetX
    }


getItemBeingDragged dragState =
    case dragState of
        DraggingStack mouse offset id ->
            Just ( StackBeingDragged, getElementOffsets mouse offset, id )

        DraggingItem mouse offset id ->
            Just ( ItemBeingDragged, getElementOffsets mouse offset, id )

        DraggingBoard mouse offset id ->
            Just ( BoardBeingDragged, getElementOffsets mouse offset, id )

        _ ->
            Nothing


handleMouseMove : DragState -> MouseMoveEvent -> DragState
handleMouseMove dragState newMousePosition =
    case ( dragState, newMousePosition.buttons ) of
        ( DraggingStack _ offsets id, 1 ) ->
            DraggingStack newMousePosition offsets id

        ( ItemPressedNotYetMoved _ offsets id, 1 ) ->
            DraggingItem newMousePosition offsets id

        ( DraggingItem _ offsets id, 1 ) ->
            DraggingItem newMousePosition offsets id

        ( BoardPressedNotYetMoved _ offsets id, 1 ) ->
            DraggingBoard newMousePosition offsets id

        ( DraggingBoard _ offsets id, 1 ) ->
            DraggingBoard newMousePosition offsets id

        -- Any registered mouse move without mouse pressed is ending eny drag session
        ( _, 0 ) ->
            NoDrag

        _ ->
            dragState


handleItemMouseDown : String -> MouseDownEvent -> DragState
handleItemMouseDown itemId { mousePosition, offsets } =
    ItemPressedNotYetMoved mousePosition offsets itemId


handleBoardMouseDown : String -> MouseDownEvent -> DragState
handleBoardMouseDown boardId { mousePosition, offsets } =
    BoardPressedNotYetMoved mousePosition offsets boardId


handleStackTitleMouseDown : String -> MouseDownEvent -> DragState
handleStackTitleMouseDown stackId { mousePosition, offsets } =
    DraggingStack mousePosition offsets stackId


handleItemEnter dragState itemUnder stacks =
    case dragState of
        DraggingItem _ _ itemOver ->
            if itemOver == itemUnder then
                Nothing

            else
                Just (moveItem stacks { from = itemOver, to = itemUnder })

        _ ->
            Nothing


handleStackEnter dragState stackUnder boards =
    case dragState of
        DraggingStack _ _ stackOver ->
            if stackOver == stackUnder then
                Nothing

            else
                Just (moveItem boards { from = stackOver, to = stackUnder })

        _ ->
            Nothing


handleBoardEnter : DragState -> String -> List String -> Maybe (List String)
handleBoardEnter dragState boardUnder boards =
    case dragState of
        DraggingBoard _ _ boardOver ->
            Just (moveItemInList boards { from = boardOver, to = boardUnder })

        _ ->
            Nothing


handleStackOverlayEnter dragState stackUnder partialModel =
    case dragState of
        DraggingStack _ _ stackOver ->
            Just { partialModel | boards = moveItem partialModel.boards { from = stackOver, to = stackUnder } }

        DraggingItem _ _ itemOver ->
            Just { partialModel | stacks = moveItemToEnd partialModel.stacks { itemToMove = itemOver, targetParent = stackUnder } }

        _ ->
            Nothing


shouldListenToMouseMoveEvents dragState =
    dragState /= NoDrag


handleMouseUp : DragState -> ( DragState, Maybe String )
handleMouseUp dragState =
    case dragState of
        ItemPressedNotYetMoved _ _ id ->
            ( NoDrag, Just id )

        _ ->
            ( NoDrag, Nothing )


isDraggingStack : DragState -> String -> Bool
isDraggingStack dragState stackId =
    case dragState of
        DraggingStack _ _ stackBeingDragged ->
            stackBeingDragged == stackId

        _ ->
            False


isDraggingBoard : DragState -> String -> Bool
isDraggingBoard dragState boardId =
    case dragState of
        DraggingBoard _ _ boardBeingDragged ->
            boardBeingDragged == boardId

        _ ->
            False


isDraggingAnyBoard : DragState -> Bool
isDraggingAnyBoard dragState =
    case dragState of
        DraggingBoard _ _ _ ->
            True

        _ ->
            False


isDraggingItem : DragState -> String -> Bool
isDraggingItem dragState id =
    case dragState of
        DraggingItem _ _ itemBeingDragged ->
            itemBeingDragged == id

        _ ->
            False


isDraggingAnything : DragState -> Bool
isDraggingAnything dragState =
    case dragState of
        NoDrag ->
            False

        ItemPressedNotYetMoved _ _ _ ->
            False

        _ ->
            True


init =
    NoDrag
