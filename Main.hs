module Main where

import SubsetSum(ssTest)
import NQueens(prettyNQueens)
import KnightsTour(prettyKnightsTour)
import Sudoku(prettySudoku, puzzle1, puzzle2, puzzle3)

main :: IO ()
main = do
  print "Start"
  -- ssTest
  -- prettyNQueens
  prettyKnightsTour
  -- prettySudoku puzzle1
  -- prettySudoku puzzle2
  -- prettySudoku puzzle3
  print "End"
