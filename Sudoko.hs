{-
This is needed
A) to use type synonyms for the state type in our instance of the BackTrack type class and 
B) for the status type to be a nested type 
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances
-}
{-# LANGUAGE FlexibleInstances #-}

module Sudoku(sudoku, prettySudoku, puzzle1, puzzle2, puzzle3) where

import BT
import Data.List(sort)
import SortedList(isDup)
import GridStrings(fillGrid4)

type Val = Int
type Position = (Int, Int)
type CellState = (Val, Bool)
type Cell = (Position, CellState)
type SudokuState = (Val, Position, [Cell])

prettySudoku pvs =  putStrLn $ 
  "Solve this puzzle:\n" ++
  (fillGrid4 9 pvs) ++ 
  "\nIs it solved?\n" ++ 
  (fillGrid4 9 $ sudoku pvs)

puzzle1 = [((1,1),9),((4,4),9),((9,9),9)] :: [((Int, Int), Int)]

puzzle2 = 
  [((1, 2), 9), ((1, 6), 7), ((1, 7), 2), ((1, 8), 5), ((1, 9), 6), 
   ((2, 1), 3), ((2, 3), 6), ((2, 4), 2), ((2, 7), 7), ((2, 8), 8), 
   ((3, 2), 2), ((3, 9), 1), 
   ((4, 2), 4), ((4, 5), 7), ((4, 7), 8), 
   ((5, 1), 8), ((5, 3), 3), ((5, 4), 1), ((5, 6), 4), ((5, 7), 5), ((5, 9), 7), 
   ((6, 3), 2), ((6, 5), 9), ((6, 8), 3), 
   ((7, 1), 6), ((7, 8), 7), 
   ((8, 2), 7), ((8, 3), 5), ((8, 6), 1), ((8, 7), 4), ((8, 9), 8), 
   ((9, 1), 2), ((9, 2), 3), ((9, 3), 4), ((9, 4), 7), ((9, 8), 1)] :: [((Int, Int), Int)]

puzzle3 = [((1,9),9),((4,4),9),((9,9),9)] :: [((Int, Int), Int)]

{- Solves a Sudoku puzzle if it can be solved.
   Takes the initial puzzle configuration as a list of
   positions and preset values for each of the positions.
   A position is a (row, column) tuple with the first row/column
   starting at 1. A preset value is just an Int.
   E.g. the initial config could be [((1,1), 1), ((9,9), 9)]
   which is a 1 in the top left corner and a 9 in the
   bottom right.
   The return value will be the initial config if the puzzle
   can't be solved or it will be list of all positions in the
   grid each with a valid value. 
   It uses an implementation of the backtracking algo as
   defined in the BT module by defining the appropriate
   functions as an instance of the BT class. 
-}
sudoku :: [(Position, Val)] -> [(Position, Val)]
sudoku pvs
  | isBad cells = pvs
  | otherwise = map delf cells'
  where
    v = 1 :: Val
    p = (1,1) :: Position
    cells          = map setf pvs
    (_, _, cells') = backTrack (v, p, cells)

{- Examines the initial config of a Sudoku puzzle to see
   if it is "bad" i.e. if it contains any duplicates in any
   row, column or 3x3 box. -}
isBad :: [Cell] -> Bool
isBad cells  = 
  (isDup $ sort $ map rv cells) || 
  (isDup $ sort $ map cv cells) || 
  (isDup $ sort $ map bv cells)

{- Make a Cell out of (Position, Val) tuple by adding a True flag -}
setf :: (Position, Val) -> Cell
setf ((r,c),v) = ((r,c),(v,True))

{- Extract a (Position, Val) tuple out of Cell by removing the flag -}
delf :: Cell -> (Position, Val)
delf ((r,c),(v,f)) = ((r,c),(v))

{- Implementation of the necessary functions of the BT class to
   solve a Sudoku puzzle. -}
instance BT SudokuState where

  -- So we can't do anything else with this cell if there
  -- are no more values to check i.e. we've already checked
  -- values 1 to 9 OR if we have gone past the last row.
  -- The latter check is required in case we have to skip
  -- the last cell because it was preset. 
  isNoMore (v, (r,c), cs) = v > 9 || r > 9

  isSkip (v, p, cs) = elem (p, True) $ map rcf cs

  -- We are finished if we have gone past the last row.
  -- isNoMore also has this check but this is needed if
  -- we have to skip a preset value.
  -- N.B. we can't use a check to see if this is the last
  -- cell i.e. r == 9 and c == 9 because ... how do I
  -- explain this?! 
  isFinal (v, (r,c), cs) = r > 9

  -- We add a new cell to the list of valid cells so far when
  -- we find a valid value for the current cell.
  add (v, p, cs) = (v, p, (p, (v,False)):cs)

  -- Try the next value in the current cell
  nextVal (v, p, cs) = (v+1, p, cs)

  -- Move onto the next cell and at the same time set the first
  -- value to try there to be 1.
  -- When moving onto the next cellif we are in column 9 of the
  -- current row we have to move to column 1 of the next row
  -- otherwise we just move ontothe next column in the same row.
  next (v, (r,c), cs) = (1, if c == 9 then (r+1, 1) else (r, c+1), cs)

  -- The current value is valid if the same value is not in the
  -- current row, column or 3x3 box.
  isValid (v, (r,c), cs) = 
    ( notIn (r,v) rv cs ) && 
    ( notIn (c,v) cv cs ) && 
    ( notIn (b,v) bv cs ) 
      where 
        b = (d3 r, d3 c)
        notIn :: Eq a => a -> (b -> a) -> [b] -> Bool
        notIn x c2x cs = notElem x $ map c2x cs 

{- A series of convenience functions to extract info from a Cell
   in a format more amenable to checks in the functions above -}
rv :: Cell -> (Int, Val)
rv ((r,c),(v,f)) = (r,v)
cv :: Cell -> (Int, Val)
cv ((r,c),(v,f)) = (c,v)
bv :: Cell -> (Position, Val)
bv ((r,c),(v,f)) = ((d3 r,d3 c),v)
rcf :: Cell -> (Position, Bool)
rcf ((r,c),(v,f)) = ((r,c),f)

{- Convenience function to return the x or y "position" of a 
   sudoku 3x3 box given the x or y dimension of the suoku grid -}
d3 :: Int -> Int
d3 n = 1 + ((n-1) // 3)

{- The div function as an infix operator -}
(//) = div