module Engine.Perft
    ( perft
    , perftDivide
    ) where

import Engine.Types
import Engine.Moves
import Engine.Legal

perft :: Int -> Position -> Int
perft 0 _   = 1
perft n pos = sum [ perft (n - 1) (makeMove m pos) | m <- legalMoves pos ]

perftDivide :: Int -> Position -> [(Move, Int)]
perftDivide n pos =
    [ (m, perft (n - 1) (makeMove m pos)) | m <- legalMoves pos ]
