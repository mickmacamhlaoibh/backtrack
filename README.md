# backtrack

Generic strategy for back tracking in Haskell. This is a Haskell 
class called `BT` in `BT.hs` where the generic strategy is 
implemented by the `backTrack` function.
  
Solutions to specific backtracking problems like the Knights Tour, 
N-Queens, Subset Sum and Sudoko are in `KnightsTour.hs`, 
`NQueens.hs`, `SubsetSum.hs` and `Sudoko.hs`.
  
## The BT class

## The backTrack Function

The default implementation of the backTrack function
is a generic strategy for back tracking where you
implement the other class functions - see below - when creating 
an instance that solves a specific back tracking problem.

### Other Class Functions 

You will need to implement these in the instance you create to
solve a specific backtracking problem. See the source code of 
the  solutions to the Knights Tour, etc. for examples of how to 
do this. Also check out the extensive comments in `BT.hs`. Note
two of these functions have default implementations that are 
often enough for a specific problem: `isSkip` and `add`.


```
isNoMore :: s -> Bool

isSkip :: s -> Bool
isSkip s = False

isValid :: s -> Bool

nextVal :: s -> s

add :: s -> s
add s = s

next :: s -> s
  
isFinal :: s -> Bool
```
  
## State
Everything about the current state of things is kept in
one place indicated by the type variable "s". So
for a specific problem you make up a state type that 
contains all the necessary info needed by the "other" functions
mentioned so that the generic backTrack function will work.
  
E.g for N queens, we could keep the following the state.

1. the value of the current column and row we are checking 
     to place a queen 

2. the positions (row and column) of previously validly placed
     queens

3. the size of the chessboard if we want to try other options
     rather than the normal 8x8 - What happens with a 2x2, 3x3
     or 4x4 board? What is the minimum size board where queens 
     can be placed? Are there "gaps" in board size i.e. it can
     be done for 1x1 but not again until when?

What other info do we need to keep?

Looking at the functions in the class BT in BT.hs and at the 
example solutions to N Queens, etc. will help.
