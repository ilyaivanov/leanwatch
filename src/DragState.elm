module DragState exposing
    ( DragState
    , ItemBeingDragged(..)
    , getItemBeingDragged
    , handleBoardEnter
    , handleBoardMouseDown
    , handleCardEnter
    , handleCardMouseDown
    , handleMouseMove
    , handleMouseUp
    , handleStackEnter
    , handleStackOverlayEnter
    , handleStackTitleMouseDown
    , init
    , isDraggingAnyBoard
    , isDraggingAnything
    , isDraggingItem
    , shouldListenToMouseMoveEvents
    )

import DictMoves exposing (moveItem, moveItemInList, moveItemToEnd)
import Utils.ExtraEvents exposing (MouseDownEvent, MouseMoveEvent, Offsets)


type DragState
    = NoDrag
    | PressedNotYetMoved ItemBeingDragged MouseMoveEvent Offsets String
    | DraggingSomething ItemBeingDragged MouseMoveEvent Offsets String


type ItemBeingDragged
    = BoardBeingDragged
    | StackBeingDragged
    | CardBeingDragged


getElementOffsets mousePosition offsets =
    { top = mousePosition.pageY - offsets.offsetY
    , left = mousePosition.pageX - offsets.offsetX
    }


getItemBeingDragged dragState =
    case dragState of
        DraggingSomething itemBeingDragged mouse offset id ->
            Just ( itemBeingDragged, getElementOffsets mouse offset, id )

        _ ->
            Nothing


handleMouseMove : DragState -> MouseMoveEvent -> DragState
handleMouseMove dragState newMousePosition =
    case ( dragState, newMousePosition.buttons ) of
        ( DraggingSomething itemType _ offsets id, 1 ) ->
            DraggingSomething itemType newMousePosition offsets id

        ( PressedNotYetMoved itemType _ offsets id, 1 ) ->
            DraggingSomething itemType newMousePosition offsets id

        -- Any registered mouse move without mouse pressed is ending eny drag session
        ( _, 0 ) ->
            NoDrag

        _ ->
            dragState


handleCardMouseDown : String -> MouseDownEvent -> DragState
handleCardMouseDown cardId { mousePosition, offsets } =
    PressedNotYetMoved CardBeingDragged mousePosition offsets cardId


handleBoardMouseDown : String -> MouseDownEvent -> DragState
handleBoardMouseDown boardId { mousePosition, offsets } =
    PressedNotYetMoved BoardBeingDragged mousePosition offsets boardId


handleStackTitleMouseDown : String -> MouseDownEvent -> DragState
handleStackTitleMouseDown stackId { mousePosition, offsets } =
    PressedNotYetMoved StackBeingDragged mousePosition offsets stackId


handleCardEnter dragState cardUnderId stacks =
    case dragState of
        DraggingSomething itemType _ _ itemOverId ->
            if itemOverId == cardUnderId || itemType /= CardBeingDragged then
                Nothing

            else
                Just (moveItem stacks { from = itemOverId, to = cardUnderId })

        _ ->
            Nothing


handleStackEnter dragState stackUnderId boards =
    case dragState of
        DraggingSomething itemType _ _ itemOverId ->
            if itemOverId == stackUnderId || itemType /= StackBeingDragged then
                Nothing

            else
                Just (moveItem boards { from = itemOverId, to = stackUnderId })

        _ ->
            Nothing


handleBoardEnter : DragState -> String -> List String -> Maybe (List String)
handleBoardEnter dragState boardUnder boards =
    case dragState of
        DraggingSomething itemType _ _ itemOverId ->
            case itemType of
                BoardBeingDragged ->
                    Just (moveItemInList boards { from = itemOverId, to = boardUnder })

                _ ->
                    Nothing

        _ ->
            Nothing


handleStackOverlayEnter dragState stackUnder partialModel =
    case dragState of
        DraggingSomething itemType _ _ itemOverId ->
            case itemType of
                StackBeingDragged ->
                    Just { partialModel | boards = moveItem partialModel.boards { from = itemOverId, to = stackUnder } }

                CardBeingDragged ->
                    Just { partialModel | stacks = moveItemToEnd partialModel.stacks { itemToMove = itemOverId, targetParent = stackUnder } }

                BoardBeingDragged ->
                    Nothing

        _ ->
            Nothing


shouldListenToMouseMoveEvents dragState =
    dragState /= NoDrag


handleMouseUp : DragState -> ( DragState, Maybe String )
handleMouseUp dragState =
    case dragState of
        PressedNotYetMoved _ _ _ id ->
            ( NoDrag, Just id )

        _ ->
            ( NoDrag, Nothing )


isDraggingItem : DragState -> String -> Bool
isDraggingItem dragState id =
    case dragState of
        DraggingSomething _ _ _ itemBeingDragged ->
            itemBeingDragged == id

        _ ->
            False


isDraggingAnyBoard : DragState -> Bool
isDraggingAnyBoard dragState =
    case dragState of
        DraggingSomething itemBeingDragged _ _ _ ->
            itemBeingDragged == BoardBeingDragged

        _ ->
            False


isDraggingAnything : DragState -> Bool
isDraggingAnything dragState =
    case dragState of
        DraggingSomething _ _ _ _ ->
            True

        _ ->
            False


init =
    NoDrag
