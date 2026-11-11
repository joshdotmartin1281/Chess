module Engine.MoveParser
    ( parseMove
    , parseLegalMove
    , showMove
    ) where

import Data.Char (toLower)

import Engine.Types
import Engine.Legal


parseMove :: String -> Maybe Move
parseMove s =
    case s of
        [a, b, c, d] ->
            Move <$> parseSquare [a, b] <*> parseSquare [c, d] <*> pure Nothing
        [a, b, c, d, p] ->
            Move <$> parseSquare [a, b] <*> parseSquare [c, d]
                 <*> fmap Just (parsePromotion p)
        _ -> Nothing


parseSquare :: String -> Maybe Square
parseSquare [f, r] = do
    fl <- lookup (toLower f) (zip "abcdefgh" [A ..])
    rk <- lookup r (zip "12345678" [R1 ..])
    Just (Square fl rk)
parseSquare _ = Nothing


parsePromotion :: Char -> Maybe PieceType
parsePromotion c =
    lookup (toLower c)
        [('q', Queen), ('r', Rook), ('b', Bishop), ('n', Knight)]


parseLegalMove :: Position -> String -> Either String Move
parseLegalMove pos input =
    case parseMove (filter (/= ' ') input) of
        Nothing ->
            Left "Could not parse move (expected something like e2e4 or e7e8q)"
        Just m
            | m `elem` legal ->
                Right m
            | promotionMissing m ->
                Left "Promotion piece required (e.g. e7e8q)"
            | otherwise ->
                Left "Illegal move"
  where
    legal = legalMoves pos

    promotionMissing m =
        promotion m == Nothing
        && any (\l -> from l == from m && to l == to m) legal


showMove :: Move -> String
showMove (Move f t p) =
    showSquare f ++ showSquare t ++ maybe "" showPromotion p
  where
    showSquare (Square fl rk) =
        map toLower (show fl) ++ show (fromEnum rk + 1)

    showPromotion Queen  = "q"
    showPromotion Rook   = "r"
    showPromotion Bishop = "b"
    showPromotion Knight = "n"
    showPromotion _      = ""
