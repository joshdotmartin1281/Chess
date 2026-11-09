module Main where

import Engine.Types
import Engine.Board
import Engine.Moves
import Engine.Render
import Engine.Legal
import Engine.Perft

main :: IO ()
main = putStrLn (renderBoard initialBoard)

