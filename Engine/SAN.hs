module Engine.SAN
    ( parseSAN
    , showSAN
    ) where

import Data.Char (isDigit, toUpper)

import Engine.Types
import Engine.Board
import Engine.Moves
import Engine.Legal


data Disambig
    = NoDis
    | DisFile File
    | DisRank Rank
    | DisSquare Square


parseSAN :: Position -> String -> Either String Move
parseSAN pos input
    | s == "O-O"   = castle G
    | s == "O-O-O" = castle C
    | otherwise    = parseNormal pos s
  where
    s = map (\c -> if c == '0' then 'O' else c)
            (filter (`notElem` "+#!? ") input)

    castle target =
        case [ m
             | m <- legalMoves pos
             , pieceAt (from m) (board pos) == Just (Piece (sideToMove pos) King)
             , file (from m) == E
             , file (to m) == target
             ] of
            [m] -> Right m
            _   -> Left "Castling is not legal here"


parseNormal :: Position -> String -> Either String Move
parseNormal pos s = do
    let (body, promo) = splitPromotion s
        (pt, rest)    = splitPiece body
        rest'         = filter (/= 'x') rest
        n             = length rest'

    if n < 2 then Left "Could not parse move" else Right ()

    dest <- maybe (Left "Bad destination square") Right
                  (sanSquare (drop (n - 2) rest'))
    dis  <- parseDisambig (take (n - 2) rest')

    let b  = board pos
        us = sideToMove pos

        base =
            [ m
            | m <- legalMoves pos
            , pieceAt (from m) b == Just (Piece us pt)
            , to m == dest
            , matches dis (from m)
            , pawnFileOk pt dis m
            ]

        cands = filter ((== promo) . promotion) base

    case cands of
        [m] -> Right m
        []  | promo == Nothing && not (null base) ->
                Left "Promotion piece required (e.g. e8=Q)"
            | otherwise ->
                Left "No legal move matches"
        _   -> Left "Ambiguous move"
  where
    pawnFileOk Pawn NoDis m = file (from m) == file (to m)
    pawnFileOk _    _     _ = True


splitPromotion :: String -> (String, Maybe PieceType)
splitPromotion s =
    case reverse s of
        (p : '=' : rest)
            | Just t <- promoType (toUpper p) -> (reverse rest, Just t)
        (p : rest@(d : _))
            | isDigit d, Just t <- promoType p -> (reverse rest, Just t)
        _ -> (s, Nothing)
  where
    promoType c =
        lookup c [('Q', Queen), ('R', Rook), ('B', Bishop), ('N', Knight)]


splitPiece :: String -> (PieceType, String)
splitPiece (c : cs)
    | Just t <- lookup c pieces = (t, cs)
  where
    pieces =
        [('N', Knight), ('B', Bishop), ('R', Rook), ('Q', Queen), ('K', King)]
splitPiece s = (Pawn, s)


parseDisambig :: String -> Either String Disambig
parseDisambig "" = Right NoDis
parseDisambig [c]
    | Just f <- lookup c fileTable = Right (DisFile f)
    | Just r <- lookup c rankTable = Right (DisRank r)
parseDisambig [f, r] =
    maybe (Left "Bad disambiguation") (Right . DisSquare) (sanSquare [f, r])
parseDisambig _ = Left "Bad disambiguation"


matches :: Disambig -> Square -> Bool
matches NoDis         _  = True
matches (DisFile f)   sq = file sq == f
matches (DisRank r)   sq = rank sq == r
matches (DisSquare s) sq = sq == s


fileTable :: [(Char, File)]
fileTable = zip "abcdefgh" [A ..]


rankTable :: [(Char, Rank)]
rankTable = zip "12345678" [R1 ..]


sanSquare :: String -> Maybe Square
sanSquare [f, r] = do
    fl <- lookup f fileTable
    rk <- lookup r rankTable
    Just (Square fl rk)
sanSquare _ = Nothing


showSAN :: Position -> Move -> String
showSAN pos m = body ++ suffix
  where
    b      = board pos
    us     = sideToMove pos
    mover  = pieceAt (from m) b
    isCapture = pieceAt (to m) b /= Nothing
    destStr   = squareStr (to m)

    fileDist =
        abs (fromEnum (file (from m)) - fromEnum (file (to m)))

    body =
        case mover of
            Just (Piece _ King)
                | fileDist == 2 ->
                    if file (to m) == G then "O-O" else "O-O-O"
            Just (Piece _ Pawn) ->
                pawnPart
            Just (Piece _ t) ->
                pieceLetter t : disambig ++ (if isCapture then "x" else "") ++ destStr
            Nothing ->
                destStr

    pawnPart =
        (if file (from m) /= file (to m)
            then [fileChar (from m), 'x']
            else "")
        ++ destStr
        ++ maybe "" (\t -> ['=', pieceLetter t]) (promotion m)

    others =
        [ o
        | o <- legalMoves pos
        , from o /= from m
        , to o == to m
        , pieceAt (from o) b == mover
        ]

    disambig
        | null others = ""
        | all (\o -> file (from o) /= file (from m)) others = [fileChar (from m)]
        | all (\o -> rank (from o) /= rank (from m)) others = rankStr (from m)
        | otherwise = squareStr (from m)

    suffix =
        let after = makeMove m pos
        in if inCheck (opposite us) after
           then (if null (legalMoves after) then "#" else "+")
           else ""


pieceLetter :: PieceType -> Char
pieceLetter Pawn   = 'P'
pieceLetter Knight = 'N'
pieceLetter Bishop = 'B'
pieceLetter Rook   = 'R'
pieceLetter Queen  = 'Q'
pieceLetter King   = 'K'


fileChar :: Square -> Char
fileChar sq = "abcdefgh" !! fromEnum (file sq)


rankStr :: Square -> String
rankStr sq = show (fromEnum (rank sq) + 1)


squareStr :: Square -> String
squareStr sq = fileChar sq : rankStr sq
