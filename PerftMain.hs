module Main where

import Engine.Board
import Engine.Perft

main :: IO ()
main = mapM_ run [1 .. 5]
  where
    run d = putStrLn (show d ++ ": " ++ show (perft d initialPosition))
