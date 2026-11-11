module Engine.Fen
    ( parseFen
    ) where

import Data.Char (isDigit, digitToInt, isUpper, toLower)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Engine.Types
import Engine.Board


parseFen :: String -> Maybe Position
parseFen s =
    case words s of
        (placement : side : castle : ep : rest) -> do
            b <- parseBoard placement
            c <- parseSide side
            e <- parseEp ep
            let (hm, fm) =
                    case rest of
                        (h : f : _) ->
                            ( fromMaybe 0 (readMaybe h)
                            , fromMaybe 1 (readMaybe f)
                            )
                        _ -> (0, 1)
            Just Position
                { board           = b
                , sideToMove      = c
                , castlingRights  = parseCastling castle
                , enPassantTarget = e
                , halfmoveClock   = hm
                , fullmoveNumber  = fm
                }
        _ -> Nothing


parseBoard :: String -> Maybe Board
parseBoard str
    | length rows /= 8 = Nothing
    | otherwise = do
        placed <- sequence (zipWith parseRow [R8, R7 .. R1] rows)
        Just (foldr (\(sq, p) b -> placePiece sq p b) emptyBoard (concat placed))
  where
    rows = splitOn '/' str


parseRow :: Rank -> String -> Maybe [(Square, Piece)]
parseRow r = go 0
  where
    go :: Int -> String -> Maybe [(Square, Piece)]
    go 8 [] = Just []
    go _ [] = Nothing
    go n (c : cs)
        | isDigit c =
            let k = digitToInt c
            in if k >= 1 && n + k <= 8 then go (n + k) cs else Nothing
        | n >= 8 = Nothing
        | otherwise = do
            p    <- parsePiece c
            rest <- go (n + 1) cs
            Just ((Square (toEnum n) r, p) : rest)


parsePiece :: Char -> Maybe Piece
parsePiece c = do
    t <- lookup (toLower c)
            [ ('p', Pawn), ('n', Knight), ('b', Bishop)
            , ('r', Rook), ('q', Queen),  ('k', King) ]
    Just (Piece (if isUpper c then White else Black) t)


parseSide :: String -> Maybe Color
parseSide "w" = Just White
parseSide "b" = Just Black
parseSide _   = Nothing


parseCastling :: String -> CastlingRights
parseCastling s = CastlingRights
    { whiteKingSide  = 'K' `elem` s
    , whiteQueenSide = 'Q' `elem` s
    , blackKingSide  = 'k' `elem` s
    , blackQueenSide = 'q' `elem` s
    }


parseEp :: String -> Maybe (Maybe Square)
parseEp "-" = Just Nothing
parseEp [f, r] = do
    fl <- lookup f (zip "abcdefgh" [A ..])
    rk <- lookup r (zip "12345678" [R1 ..])
    Just (Just (Square fl rk))
parseEp _ = Nothing


splitOn :: Char -> String -> [String]
splitOn d s =
    case break (== d) s of
        (a, [])       -> [a]
        (a, _ : rest) -> a : splitOn d rest
