module Main where

import Engine.Fen
import Engine.Perft
import Engine.Types

suite :: [(String, String, [Int])]
suite =
    [ ( "Start position"
      , "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      , [20, 400, 8902, 197281]
      )
    , ( "Kiwipete"
      , "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
      , [48, 2039, 97862]
      )
    , ( "Position 3 (endgame, en passant pins)"
      , "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
      , [14, 191, 2812, 43238]
      )
    , ( "Position 4 (promotions, castling rights)"
      , "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
      , [6, 264, 9467]
      )
    , ( "Position 5 (promotion with check)"
      , "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"
      , [44, 1486, 62379]
      )
    ]


main :: IO ()
main = mapM_ runCase suite

runCase :: (String, String, [Int]) -> IO ()
runCase (name, fen, expected) = do
    putStrLn name
    case parseFen fen of
        Nothing  -> putStrLn "  FEN parse failed"
        Just pos ->
            mapM_ (check pos) (zip [1 ..] expected)

check :: Position -> (Int, Int) -> IO ()
check pos (d, want) = do
    let got = perft d pos
    putStrLn $ "  depth " ++ show d ++ ": " ++ show got
        ++ (if got == want then "  ok" else "  MISMATCH (expected " ++ show want ++ ")")
