module Engine.MoveGen
    ( pieceMoves
    , allMoves
    , opposite
    , pawnDirection
    , offsetSquare
    , knightOffsets
    , bishopOffsets
    , rookOffsets
    , kingOffsets
    ) where

import Engine.Types
import Engine.Board


startRank :: Color -> Rank
startRank White = R2
startRank Black = R7


opposite :: Color -> Color
opposite White = Black
opposite Black = White


pawnDirection :: Color -> Int
pawnDirection White = 1
pawnDirection Black = -1


promotionRank :: Color -> Rank
promotionRank White = R8
promotionRank Black = R1


promotionMoves :: Square -> Square -> [Move]
promotionMoves from to =
    [ Move from to (Just Queen)
    , Move from to (Just Rook)
    , Move from to (Just Bishop)
    , Move from to (Just Knight)
    ]


captureOffsets :: Color -> [(Int, Int)]
captureOffsets color =
    [(-1, pawnDirection color), (1, pawnDirection color)]


pawnMoves :: Position -> Color -> Square -> [Move]
pawnMoves pos color from =
    singleMove ++ doubleMove ++ captureMoves ++ enPassantMoves
  where
    mkMoves to
        | rank to == promotionRank color =
            promotionMoves from to
        | otherwise =
            [Move from to Nothing]

    singleMove =
        case offsetSquare from (0, pawnDirection color) of
            Just to
                | pieceAt to (board pos) == Nothing ->
                    mkMoves to
            _ ->
                []

    doubleMove =
        case from of
            Square _ r
                | r == startRank color ->
                    case ( offsetSquare from (0, pawnDirection color)
                         , offsetSquare from (0, 2 * pawnDirection color) ) of
                        (Just one, Just two)
                            | pieceAt one (board pos) == Nothing
                            , pieceAt two (board pos) == Nothing ->
                                [Move from two Nothing]
                        _ ->
                            []
            _ ->
                []

    enPassantMoves =
        case enPassantTarget pos of
            Nothing ->
                []

            Just target ->
                [ Move from target Nothing
                | offset <- captureOffsets color
                , offsetSquare from offset == Just target
                , let capturedSquare = Square (file target) (rank from)
                , pieceAt capturedSquare (board pos) == Just (Piece (opposite color) Pawn)
                ]

    captureMoves =
        concat
            [ mkMoves to
            | offset <- captureOffsets color
            , Just to <- [offsetSquare from offset]
            , Just (Piece c _) <- [pieceAt to (board pos)]
            , c /= color
            ]


knightOffsets :: [(Int, Int)]
knightOffsets =
    [ (1,2)
    , (2,1)
    , (2,-1)
    , (1,-2)
    , (-1,-2)
    , (-2,-1)
    , (-2,1)
    , (-1,2)
    ]


knightMoves :: Position -> Color -> Square -> [Move]
knightMoves pos color fromSq =
    [ Move fromSq toSq Nothing
    | offset <- knightOffsets
    , Just toSq <- [offsetSquare fromSq offset]
    , canMoveTo pos color toSq
    ]


bishopOffsets :: [(Int, Int)]
bishopOffsets =
    [ (1,1)
    , (1,-1)
    , (-1,1)
    , (-1,-1)
    ]


bishopMoves :: Position -> Color -> Square -> [Move]
bishopMoves pos color fromSq =
    concatMap (ray pos color fromSq) bishopOffsets


rookOffsets :: [(Int, Int)]
rookOffsets =
    [ (1,0)
    , (0,1)
    , (-1,0)
    , (0,-1)
    ]


rookMoves :: Position -> Color -> Square -> [Move]
rookMoves pos color fromSq =
    concatMap (ray pos color fromSq) rookOffsets


queenOffsets :: [(Int, Int)]
queenOffsets =
    bishopOffsets ++ rookOffsets


queenMoves :: Position -> Color -> Square -> [Move]
queenMoves pos color fromSq =
    concatMap (ray pos color fromSq) queenOffsets


kingOffsets :: [(Int, Int)]
kingOffsets =
    bishopOffsets ++ rookOffsets


kingStartSquare :: Color -> Square
kingStartSquare White = Square E R1
kingStartSquare Black = Square E R8


kingSideTarget :: Color -> Square
kingSideTarget White = Square G R1
kingSideTarget Black = Square G R8


queenSideTarget :: Color -> Square
queenSideTarget White = Square C R1
queenSideTarget Black = Square C R8


kingSideRookSquare :: Color -> Square
kingSideRookSquare White = Square H R1
kingSideRookSquare Black = Square H R8


kingSideSquares :: Color -> [Square]
kingSideSquares White = [Square F R1, Square G R1]
kingSideSquares Black = [Square F R8, Square G R8]


queenSideRookSquare :: Color -> Square
queenSideRookSquare White = Square A R1
queenSideRookSquare Black = Square A R8


queenSideSquares :: Color -> [Square]
queenSideSquares White = [Square B R1, Square C R1, Square D R1]
queenSideSquares Black = [Square B R8, Square C R8, Square D R8]


kingSideRight :: Color -> CastlingRights -> Bool
kingSideRight White = whiteKingSide
kingSideRight Black = blackKingSide


queenSideRight :: Color -> CastlingRights -> Bool
queenSideRight White = whiteQueenSide
queenSideRight Black = blackQueenSide


castleMoves :: Position -> Color -> Square -> [Move]
castleMoves pos color fromSq
    | fromSq /= kingStartSquare color = []
    | otherwise =
        kingSide ++ queenSide
  where
    kingSide
        | canCastleKingSide pos color =
            [Move fromSq (kingSideTarget color) Nothing]
        | otherwise = []

    queenSide
        | canCastleQueenSide pos color =
            [Move fromSq (queenSideTarget color) Nothing]
        | otherwise = []


canCastleKingSide :: Position -> Color -> Bool
canCastleKingSide pos color =
    hasRight
    && kingExists
    && rookExists
    && squaresEmpty
  where
    hasRight =
        kingSideRight color (castlingRights pos)

    kingExists =
        pieceAt (kingStartSquare color) (board pos) ==
            Just (Piece color King)

    rookExists =
        pieceAt (kingSideRookSquare color) (board pos) ==
            Just (Piece color Rook)

    squaresEmpty =
        all empty (kingSideSquares color)

    empty sq =
        pieceAt sq (board pos) == Nothing


canCastleQueenSide :: Position -> Color -> Bool
canCastleQueenSide pos color =
    hasRight
    && kingExists
    && rookExists
    && squaresEmpty
  where
    hasRight =
        queenSideRight color (castlingRights pos)

    kingExists =
        pieceAt (kingStartSquare color) (board pos) ==
            Just (Piece color King)

    rookExists =
        pieceAt (queenSideRookSquare color) (board pos) ==
            Just (Piece color Rook)

    squaresEmpty =
        all empty (queenSideSquares color)

    empty sq =
        pieceAt sq (board pos) == Nothing


kingMoves :: Position -> Color -> Square -> [Move]
kingMoves pos color fromSq =
    singleMove ++ castleMoves pos color fromSq
  where
    singleMove =
        [ Move fromSq toSq Nothing
        | offset <- kingOffsets
        , Just toSq <- [offsetSquare fromSq offset]
        , canMoveTo pos color toSq
        ]


pieceMoves :: Position -> Square -> [Move]
pieceMoves pos sq =
    case pieceAt sq (board pos) of
        Nothing -> []
        Just (Piece color Pawn)   -> pawnMoves pos color sq
        Just (Piece color Knight) -> knightMoves pos color sq
        Just (Piece color Bishop) -> bishopMoves pos color sq
        Just (Piece color Rook)   -> rookMoves pos color sq
        Just (Piece color Queen)  -> queenMoves pos color sq
        Just (Piece color King)   -> kingMoves pos color sq


allMoves :: Position -> [Move]
allMoves pos =
    concatMap (pieceMoves pos) ownPieces
  where
    ownPieces =
        filter isMine (occupiedSquares (board pos))

    isMine sq =
        case pieceAt sq (board pos) of
            Just (Piece c _) -> c == sideToMove pos
            Nothing -> False


ray :: Position -> Color -> Square -> (Int, Int) -> [Move]
ray pos color fromSq dir = go fromSq
  where
    go sq =
      case offsetSquare sq dir of
        Nothing -> []

        Just next ->
          case pieceAt next (board pos) of
            Nothing ->
              Move fromSq next Nothing : go next

            Just (Piece c _)
              | c /= color -> [Move fromSq next Nothing]
              | otherwise  -> []


canMoveTo :: Position -> Color -> Square -> Bool
canMoveTo pos color sq =
    case pieceAt sq (board pos) of
        Nothing -> True
        Just (Piece c _) -> c /= color


offsetFile :: File -> Int -> Maybe File
offsetFile f d =
    let n = fromEnum f + d
    in if n >= fromEnum (minBound :: File)
          && n <= fromEnum (maxBound :: File)
       then Just (toEnum n)
       else Nothing


offsetRank :: Rank -> Int -> Maybe Rank
offsetRank r d =
    let n = fromEnum r + d
    in if n >= fromEnum (minBound :: Rank)
          && n <= fromEnum (maxBound :: Rank)
       then Just (toEnum n)
       else Nothing


offsetSquare :: Square -> (Int, Int) -> Maybe Square
offsetSquare (Square f r) (df, dr) = do
    f' <- offsetFile f df
    r' <- offsetRank r dr
    pure (Square f' r')
