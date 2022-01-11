module BT where

{-
  Generic strategy for back tracking

  The default implementation of the backTrack function
  is a generic strategy for back tracking where you
  implement the other functions when creating an instance.
  The other functions are specific to the problem as you will
  see in the comments below.

  Everything about the current state of things is kept in
  one place indicated by the type variable "s" below. So
  for a specific problem you make up a state type that 
  contains all the necessary info needed by the functions
  below so that the generic backTrack function will work.
  
  E.g for N queens, we could keep the following the state.

  A) the value of the current column and row we are checking 
     to place a queen 

  B) the positions (row and column) of previously validly placed
     queens

  C) the size of the chessboard if we want to try other options
     rather than the normal 8x8 - What happens with a 2x2, 3x3
     or 4x4 board? What is the minimum size board where queens 
     can be placed? Are there "gaps" in board size i.e. it can
     be done for 1x1 but not again until when?

  What other info do we need to keep?

  Looking at the functions below will help.
-}  
class BT s where

  {-
    Returns true if we have no more options that can be checked 
    within the current state.
    This is the "backtrack" condition i.e. we have run into a 
    dead-end and have to go back and try another option in a 
    previous state if one exists.
    E.g. 
    For N queens, this could be no more rows in the current column.  
    For Sudoku, this could be no more values i.e. we have tried all
    the values from 1 to 9.
    -}
  isNoMore :: s -> Bool
  
  {-
    Returns true if the current state should be skipped.
    Doesn't apply to N queens so the default implementation
    should be used i.e. always return false
    But it would apply to Sudoku where there is preset value
    already in the current cell from the intial setup of the
    puzzle.
  -}
  isSkip :: s -> Bool
  isSkip s = False

  {-
    Returns true if the value is valid when checked against the 
    state.
    For N queens, this could mean that the current row for the 
    current column is in a different row, column and diagonal than 
    any other queen recorded within the state.
    For Sudoku, this could be checking that the current value doesn't
    already exist in the same row, column or box.
  -}
  isValid :: s -> Bool
  
  {-
    If we find that the current value is not valid for the 
    current state, then we need to move onto the "next" value 
    in this state.
    E.g. 
    For N queens, this could be the next row in the current 
    column so update the state to move onto the next row.
    For Sudoku, this could be trying the next value in the current cell
  -}
  nextVal :: s -> s
  
  {-
    If we find the current value is valid then we add it and any other
    relevant info to the current state      
    E.g. 
    For N qeeens, add the current valid position - row AND columnn
    to the valid positions already found
    For Sudoku, add the current value to the current cell
    In some scenarios the default implementation can be used
    because the next function effectively does the adding
    and its function does not need to be separated from the
    add function e.g. the "subset sum" problem
  -}
  add :: s -> s
  add s = s

  {-
    Move onto to the next state
    e.g. 
    For N queens, move onto the first row in the next column 
    For Sudoku, move onto the next cell with 1 as the first value to check 
  -}
  next :: s -> s
  
  {-
    Returns true if this is the final state 
    e.g. 
    For N queens, we've filled the last column
    For Sudoku, we've filled all cells
  -}
  isFinal :: s -> Bool
  
  {- 
    This is the default implementation of our back tracking algo.
    Hopefully it's generic enough to handle all backtracking scenarios.
    So in 99% of cases you just leave this alone otherwise what would be
    the point of this class.
    N.B. The order of the guards is VERY important!
  -}  
  backTrack :: s -> s
  backTrack s
    | isNoMore s           = s
    | isSkip s             = backTrack $ next s
    | valid && isFinal s'  = s'
    | valid && isFinal s'' = s''
    | otherwise            = backTrack $ nextVal s
      where
        valid = isValid s
        s' = next $ add s
        s'' = backTrack s'