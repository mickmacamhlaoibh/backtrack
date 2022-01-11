{-
  This is needed
  A) to use type synonyms for the state type in our instance of the BackTrack type class and 
  B) for the status type to be a nested type 
  https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-FlexibleInstances
-}
{-# LANGUAGE FlexibleInstances #-}

module SubsetSum(subsetSum, ssTest) where

import BT
import Data.List(sort)
import SortedList(less,mergeAll)

ssTest = print $ subsetSum 11 [4,4,6,9]

type Sum = Int
{- *NB* The list of Ints MUST be sorted for this to work!!! 
   Fortunately, the subsetSum function sorts the list before it 
   starts its work.
-}
type Set = [Int]
{- The state is made up of the following elements:
   - The target sum we are trying to hit.
   - The sum so far - always less than or equal to the target.
   - The current subset that makes up the sum so far.
   - The remaining values to try adding to the current subset,
        this starts off as the complement of the current subset 
        and is whittled down as we try each value.
   - The complement of the current subset.
  We need to keep the full set in some form so how we do this
  in this implementation is to always keep the subset that 
  makes up the sum so far and its complement.
-}
type SubsetState = (Sum, Sum, Set, Set, Set)

{- Try to find the subset in the specified set whose sum
   is equal to the target.
   Returns a tuple (target, subset) where the target is the
   original target and the subset is either empty if no 
   solution was found or is the subset whose sum makes up
   the target
   It uses an implementation of the backtracking algo as
   defined in the BT module by defining the appropriate
   functions as an instance of the BT class. 
-}
subsetSum :: Sum -> Set -> (Sum, Set)
subsetSum target set = (target, sub)
  where
    set' = sort set
    (_, _, sub, _, _) = backTrack (target, 0, [], set', set') :: SubsetState

instance BT SubsetState where

  -- We can't try any more values when they're all gone!
  isNoMore (_, _, _, [], _) = True
  -- This works because we assume r:rs is sorted
  -- Otherwise we'd have to return True i.e.
  -- go through every element in r:rs but
  -- because it's sorted we can stop if the next
  -- r when added to s is bigger than t.
  isNoMore (t, s, _, r:rs, _) = s + r > t

  -- the current value r is valid if when added to the
  -- the sum so far it doesn't exceed the target
  isValid (t, s, _, r:rs, _) = s + r <= t

  -- We are finished successfully if the sum so far
  -- matches the target
  isFinal (t, s, _, _ , _) = s == t

  -- This does a few things, it increases the sum so far by 
  -- the next value r in the listof remaining values to try.
  -- It adds r to the the subset that currently makes up 
  -- the sum so far. It removes r from the complement of 
  -- the current subset to be the new complement. Finally,
  -- it sets the remaining values to try to be this new 
  -- new complement - it is important than we don;t just use 
  -- the current remaining values less the first one i.e. we
  -- need to start afresh in the next state with a full set
  -- of "remaining" values.
  next (t, s, c, r:rs, d) = (t, s+r, mergeAll c [r], d', d')
    where d' = d `less` [r]

  -- Try the next value by removing the first element of the
  -- remaining values.
  nextVal (t, s, c, r:rs, d) = (t, s, c, rs, d)