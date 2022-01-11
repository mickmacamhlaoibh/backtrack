{-
This is needed
A) to use type synonyms for the state type in our instance of the BackTrack type class and 
B) for the status type to be a nested type 
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances
-}
{-# LANGUAGE FlexibleInstances #-}

module KnightsTour(prettyKnightsTour, knightsTour, knightsTourGrid) where

import BT
import GridStrings(gridRows, fillGrid3)

-- prettyKnightsTour =  putStrLn $ knightsTourGrid 8
prettyKnightsTour =  putStrLn $ show $ knightsTour 8

knightsTourGrid n = fillGrid3 n $ zip ss' chars
    where 
      (_,ms,s,ss) = knightsTour n
      l = length ss
      ss' = reverse ss

chars = "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!£$%^&*()+-=_[]{};'#:@~,./<>?|"

{-
  The board state is a 4 element tuple.
  The first element is the size of the board. 
  The second element is the remaining moves to be tried from
  the current square - each move is an x y tuple.
  The third element is the current ssquare as a row column tuple.
  The fourth element is a list of squares visited so far
  as a list of row column tuples.
  Note that rows and columns start from 1
-}
type Move = (Int, Int)
type Square = (Int, Int)
type BoardState = (Int, [Move], Square, [Square])

{- Try to do a complete tour of an n x n chessboard with
   a knight visiting each square only once.
   Return a Bool to indicate whether we succeeded and
   the resulting list of squares visited in the order they
   were visited.
   Use the generic back tracking strategy contained 
   within the backTrack function of the BackTrack type class.
   To do this we need to implement the other functions in the 
   BT type class by creating an Instance of it - see below.
    To start with we have 
    A) the size of the board
    B) the listof moves for the first square
    C) the top left square as the starting position
    D) no squares visited ao far
    Then we use the backTrack function in the BackTrack 
    type class to find a solution
    PS if I don't include the BoardState type annotation Haskell
    gets confused
-}
knightsTour :: Int -> BoardState 
knightsTour n = backTrack (n,moves n s,s,[])
  where s = (1,1)

moves n p = filter (onboard n) $ map (move p) kmoves

onboard n (r,c) = r >= 1 && r <= n && c >= 1 && c <= n

move (r,c) (x,y) = (r+x, c+y)

kmoves = [(x,y) | x <- as, y <- as, (abs x) /= (abs y)]
  where as = [1,-1,2,-2]

{- 
  We implement all the functions defined in the BackTrack 
  type class except for the backTrack function which 
  contains a generic backtracking strategy and the isSkip
  function which does not apply to N queens.
-}
instance BT BoardState where

    -- We're finished with this square when there are
    -- no more moves to try
    isNoMore (_,[],_,_) = True
    isNoMore (_,m:ms,_,_) = False

    -- The square is valid if we haven't visited before
    isValid (n,m:ms,s,ss) = notElem m ss

    -- We add a value to the state by adding the current
    -- square to the list of squares already visited
    add (n,ms,s,ss) = (n,ms,s,s:ss)

    -- 
    next (n,m:ms,s,ss) = (n,moves n m,m,ss)

    -- We have completed the tour whee have visited every
    -- square on the borad i.e. the number of squares visited
    -- is equal to the size of the board.
    isFinal (n,_,_,ss) = length ss == 48 -- n*n

    -- Move onto the next move in the list of moves
    nextVal (n,m:ms,s,ss) = (n,ms,s,ss)
