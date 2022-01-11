module GridStrings(gridRows,fillGrid1,fillGrid2,fillGrid3,fillGrid4) 
where

import Data.Char(intToDigit)
import ListUtil(foldl2', replace)

{-
  gridRows n produces a String that is supposed to look 
  like a character representation of a n x n grid.
  E.g 
  gridRows 2 
  is:
  +---+---+
  |   |   |
  +---+---+
  |   |   |
  +---+---+
  Each row (line) is terminated with a newline (\n).
-}
gridRows :: Int -> String
gridRows n = (concat $ replicate n (twoRows n)) ++ (oddRow n) 

{-
  Create a String representation of an n x n grid i.e. gridRows n
  and fill each (row, col) position in a list of positions ps 
  with the value v.
  E.g 
  filldGrid1 2 [(1,1), (2,2)] 'Q' 
  is:
  +---+---+
  | Q |   |
  +---+---+
  |   | Q |
  +---+---+
-}
fillGrid1 :: Int -> [(Int, Int)] -> Char -> String
fillGrid1 n ps v = foldl (replace' v) grid is
  where
    grid = gridRows n 
    is = toGridIndices n 2 4 ps
    replace' a as i = replace as i a

{-
  Create a String representation of an n x n grid i.e. gridRows n
  and fill each (row, col) position in ps with the corresponding
  value in vs.
  E.g 
  filldGrid2 2 [(1,1), (2,2)] ['1', '4']
  is:
  +---+---+
  | 1 |   |
  +---+---+
  |   | 4 |
  +---+---+
-}
fillGrid2 :: Int -> [(Int, Int)] -> [Char] -> String
fillGrid2 n ps vs = foldl2' replace grid is vs
  where
    grid = gridRows n 
    is = toGridIndices n 2 4 ps

{-
  Create a String representation of an n x n grid i.e. gridRows n
  and fill each (row, col) position in pvs with the corresponding
  value in pvs.
  E.g 
  filldGrid3 2 [((1,1), '8'), ((2,2), '7')]
  is:
  +---+---+
  | 8 |   |
  +---+---+
  |   | 7 |
  +---+---+
-}
fillGrid3 :: Int -> [((Int, Int), Char)] -> String
fillGrid3 n pvs = fillGrid2 n ps vs
  where
    ps = map fst pvs
    vs = map snd pvs

{-
  Create a String representation of an n x n grid i.e. gridRows n
  and fill each (row, col) position in pvs with the corresponding
  char representation  of the Int value in pvs.
  E.g 
  filldGrid4 2 [((1,1), 8), ((2,2), 7)]
  is:
  +---+---+
  | 8 |   |
  +---+---+
  |   | 7 |
  +---+---+
-}
fillGrid4 :: Int -> [((Int, Int), Int)] -> String
fillGrid4 n pvs = fillGrid3 n $ map d2c pvs
  where d2c (a,b) = (a, intToDigit $ mod b 10) 

{-
Two rows of a string rep of a grid 
Each row (line) is terminated with a newline (\n).
twoRows 2 
is:
+---+---+
|   |   |
twoRows 3 
is:
+---+---+---+
|   |   |   |
-}
twoRows :: Int -> String
twoRows n = oddRow n ++ evenRow n

{-
oddRow 2 
is:
+---+---+
-}
oddRow :: Int -> String
oddRow n = (take (4*n+1) $ cycle "+---") ++ "\n"

{-
evenRow 2 
is:
|   |   |
-}
evenRow :: Int -> String
evenRow n = (take (4*n+1) $ cycle "|   ") ++ "\n"
    
{-
Converts a liast of (row, col) positions of a grid into an index 
in the String representation of the n x n grid as produced by
gridRows n. You need to pass in the size of the grid i.e. n as well 
 as two other Int values: v the vertial offset which is 2 if 
using gridRows and the horizontal offset which is 4 if using 
gridRows.
-}
toGridIndices :: Int -> Int -> Int -> [(Int, Int)] -> [Int]
toGridIndices n v h = map (toGridIndex n v h)

{-
Converts a (row, col) position of a grid into an index in the
String representation of the n x n grid as produced by gridRows n.
You need to pass in the size of the grid i.e. n as well as two other
Int values: v the vertial offset which is 2 if using gridRows
and the horizontal offset which is 4 if using gridRows.
-}
toGridIndex :: Int -> Int -> Int -> (Int, Int) -> Int 
toGridIndex n v h (r,c) = r' + c'
  where
    rl = n*h+2
    r' = rl * ( ((r-1)*v) + 1 )
    c' =  ((h+1) `div` 2) + ((c-1)*h)