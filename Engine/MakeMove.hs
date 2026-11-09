module Engine.MakeMove
    ( makeMove
    , isCastleMove
    ) where

import Engine.Board
import Engine.Types


makeMove :: Move -> Position -> Position
makeMove move pos =
    case pieceAt (from move) (board pos) of
        Nothing ->
            pos

        Just piece ->
            let
                isCapture =
                    pieceAt (to move) (board pos) /= Nothing
                        || isEnPassantCapture pos piece move

                board1 = 
                    removePiece (from move) (board pos)

                board2 =
                    applyEnPassant pos piece move board1

                board3 =
                    placePiece (to move) (placedPiece piece move) board2

                newBoard =
                    if isCastleMove piece move
                    then castleRook move board3
                    else board3

                newFullmove =
                    case sideToMove pos of
                        White -> fullmoveNumber pos
                        Black -> fullmoveNumber pos + 1

                newHalfmove =
                    case piece of
                        Piece _ Pawn -> 0 
                        _
                            | isCapture -> 0
                            | otherwise ->
                                halfmoveClock pos + 1

                newRights =
                    updateCastlingRights
                        (castlingRights pos)
                        (board pos)
                        piece
                        move

                newSide =
                    case sideToMove pos of
                        White -> Black
                        Black -> White
            in
            pos
                { board = newBoard
                , sideToMove = newSide
                , castlingRights = newRights
                , enPassantTarget = newEnPassantTarget piece move
                , halfmoveClock = newHalfmove
                , fullmoveNumber = newFullmove
                }


placedPiece :: Piece -> Move -> Piece
placedPiece (Piece color Pawn) move =
    case promotion move of
        Just pieceType ->
            Piece color pieceType
        Nothing ->
            Piece color Pawn
placedPiece piece _ = piece


applyEnPassant :: Position -> Piece -> Move -> Board -> Board
applyEnPassant pos (Piece color Pawn) move board =
    case enPassantTarget pos of
        Just target
            | target == to move ->
                case color of
                    White ->
                        removePiece
                            (Square (file target) R5)
                            board
                    Black ->
                        removePiece
                            (Square (file target) R4)
                            board
            | otherwise -> board
        Nothing -> board
applyEnPassant _ _ _ board = board


newEnPassantTarget :: Piece -> Move -> Maybe Square
newEnPassantTarget (Piece White Pawn) move =
    case (from move, to move) of
        (Square f R2, Square _ R4) ->
            Just (Square f R3)
        _ ->
            Nothing
newEnPassantTarget (Piece Black Pawn) move =
    case (from move, to move) of
        (Square f R7, Square _ R5) ->
            Just (Square f R6)
        _ ->
            Nothing
newEnPassantTarget _ _ = Nothing


isEnPassantCapture :: Position -> Piece -> Move -> Bool
isEnPassantCapture pos (Piece _ Pawn) move =
    enPassantTarget pos == Just (to move)
    && pieceAt (to move) (board pos) == Nothing
isEnPassantCapture _ _ _ = False


isCastleMove :: Piece -> Move -> Bool
isCastleMove (Piece _ King) move =
    case (from move, to move) of
        (Square E R1, Square G R1) -> True
        (Square E R1, Square C R1) -> True
        (Square E R8, Square G R8) -> True
        (Square E R8, Square C R8) -> True
        _ -> False
isCastleMove _ _ =
    False


castleRook :: Move -> Board -> Board
castleRook move board =
    case (from move, to move) of
        (Square E R1, Square G R1) ->
            moveRook (Square H R1) (Square F R1)
        (Square E R1, Square C R1) ->
            moveRook (Square A R1) (Square D R1)
        (Square E R8, Square G R8) ->
            moveRook (Square H R8) (Square F R8)
        (Square E R8, Square C R8) ->
            moveRook (Square A R8) (Square D R8)
        _ -> board
  where
    moveRook rookFrom rookTo =
        case pieceAt rookFrom board of
            Nothing -> board
            Just rook ->
                placePiece rookTo rook $
                removePiece rookFrom board


updateCastlingRights :: CastlingRights -> Board -> Piece -> Move -> CastlingRights
updateCastlingRights rights board piece move = rights2  
    where
        rights1 =
            case piece of
                Piece White King ->
                    rights
                        { whiteKingSide = False
                        , whiteQueenSide = False
                        }
                Piece Black King ->
                    rights
                        { blackKingSide = False
                        , blackQueenSide = False
                        }
                Piece _ Rook ->
                    clearRightForRookSquare (from move) rights
                _ -> rights
        rights2 =
            case pieceAt (to move) board of
                Just (Piece _ Rook) ->
                    clearRightForRookSquare (to move) rights1
                _ -> rights1


clearRightForRookSquare :: Square -> CastlingRights -> CastlingRights
clearRightForRookSquare square rights =
    case square of
        Square A R1 ->
            rights { whiteQueenSide = False }
        Square H R1 ->
            rights { whiteKingSide = False }
        Square A R8 ->
            rights { blackQueenSide = False }
        Square H R8 ->
            rights { blackKingSide = False }
        _ -> rights


