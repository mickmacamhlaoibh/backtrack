{-
This is needed
A) to use type synonyms for the state type in our instance of the BackTrack type class and 
B) for the status type to be a nested type 
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances
-}
{-# LANGUAGE FlexibleInstances #-}

module NQueens(prettyNQueens, nQueens, nQueensGrid) where

import BT
import GridStrings(gridRows, fillGrid1)

prettyNQueens =  putStrLn $ nQueensGrid 8

nQueensGrid n
  | not ok = gridRows n
  | otherwise = fillGrid1 n ps 'Q'
    where 
      (ok, ps) = nQueens n

{-
  The board state is a 3 element tuple.
  The first element is the size of the board. 
  The second element is the current row and column as a tuple.
  The third element is a list of valid positions (row, col) 
  so far as a list of tuples.
  Note that rows and columns start from 1
-}
type BoardState = (Int, (Int, Int), [(Int, Int)])

{- Place N queens on an N x N chess board so that each is in 
   its own row, column and diagonal. Use the generic back 
   tracking strategy contained within the backTrack function 
   of the BackTrack type class.
   To do this we need to implement the other functions in the 
   BT type class by creating an Instance of it - see below.
-}
nQueens :: Int -> (Bool, [(Int,Int)])
nQueens n = (c > n, reverse ps)
  where
    -- To start with we have 
    -- A) the size of the board
    -- B) the first row and column as the starting position to check
    -- C) no valid positons so far
    -- Then we use the backTrack function in the BackTrack 
    -- type class to find a solution
    -- PS if I don't include the BoardState type annotation Haskell
    -- gets confused
    (_, (r, c), ps) = backTrack (n, (1, 1), []) :: BoardState

{- 
  We implement all the functions defined in the BackTrack 
  type class except for the backTack function which 
  contains a generic backtracking strategy and the isSkip
  function which does not apply to N queens.
-}
instance BT BoardState where

    -- We're finished with checking a column when we run out of rows
    isNoMore (n, (r, c), ps) = r > n

    -- The next val to check is the next row in the current column
    nextVal (n, (r, c), ps) = (n, (r + 1, c), ps) 

    -- We add a value to the state by adding the current
    -- (row, column) to the list of valid (row, column) 
    -- positions previously found and saved within the state
    add (n, (r, c), ps) = (n, (r, c), (r, c):ps)

    -- We move onto the next column starting at the first row
    next (n, (r, c), ps) = (n, (1, c + 1), ps)

    -- This is the final state if current column is off the board
    isFinal (n, (r, c), ps) = c > n

    -- A row position in the current column is valid if there 
    -- are no other queens in the same row or in the same diagonal
    isValid (n, (r,c), ps) = (inOwnRow ps r) && (inOwnDiag ps r)
      where
        -- Go back through the previous positions and see if 
        -- any have the same row position. If not, then this 
        -- row is valid
        inOwnRow [] r = True
        inOwnRow ((r', _):ps) r
          | r' == r = False
          | otherwise = inOwnRow ps r

        -- Go back through the previous positions and see if any 
        -- are in the same diag. If not, then this is valid
        inOwnDiag [] r = True
        inOwnDiag ((r', c'):ps) r
          | abs (r' - r) == abs (c' - c) = False
          | otherwise = inOwnDiag ps r