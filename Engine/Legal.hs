module Engine.Legal
    ( makeLegalMove
    , legalMoves
    , isLegalMove
    , inCheck
    , squareAttacked
    ) where

import Engine.Board
import Engine.Types
import Engine.Moves


makeLegalMove :: Move -> Position -> Maybe Position
makeLegalMove move pos
    | isLegalMove move pos = Just (makeMove move pos)
    | otherwise            = Nothing


legalMoves :: Position -> [Move]
legalMoves pos = filter (leavesKingSafe pos) (allMoves pos)

isLegalMove :: Move -> Position -> Bool
isLegalMove move pos =
    move `elem` allMoves pos && leavesKingSafe pos move

leavesKingSafe :: Position -> Move -> Bool
leavesKingSafe pos move =
    castleSafe && not (inCheck us (makeMove move pos))
  where
    us   = sideToMove pos
    them = opposite us

    castleSafe =
        case pieceAt (from move) (board pos) of
            Just piece | isCastleMove piece move ->
                not (inCheck us pos)
                && not (squareAttacked them crossing pos)
            _ -> True

    crossing =
        Square
            (if file (to move) == G then F else D)
            (rank (from move))

findKing :: Color -> Board -> Maybe Square
findKing c b =
    case [ sq
         | sq <- occupiedSquares b
         , pieceAt sq b == Just (Piece c King)
         ] of
        (s:_) -> Just s
        []    -> Nothing


inCheck :: Color -> Position -> Bool
inCheck c pos =
    case findKing c (board pos) of
        Nothing -> False
        Just k  -> squareAttacked (opposite c) k pos


squareAttacked :: Color -> Square -> Position -> Bool
squareAttacked by sq pos =
    pawnAtt
    || knightAtt
    || kingAtt
    || slide bishopOffsets [Bishop, Queen]
    || slide rookOffsets   [Rook, Queen]
  where
    b = board pos
    d = pawnDirection by

    at s p = pieceAt s b == Just (Piece by p)

    hit p o = maybe False (\s -> at s p) (offsetSquare sq o)

    pawnAtt   = any (hit Pawn)   [(-1, -d), (1, -d)]
    knightAtt = any (hit Knight) knightOffsets
    kingAtt   = any (hit King)   kingOffsets

    slide dirs types = any (go sq) dirs
      where
        go s dir =
            case offsetSquare s dir of
                Nothing -> False
                Just n  ->
                    case pieceAt n b of
                        Nothing          -> go n dir
                        Just (Piece c t) -> c == by && t `elem` types
