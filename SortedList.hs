-- Functions that operate on sorted lists.
-- The effect is primarily to make a sorted
-- list act mostly like a set.
module SortedList where

-- | \(\mathcal{O}(n)\). The 'uniq' function removes 
-- duplicate elements from a sorted list. In particular, 
-- it keeps only the first occurrence of each element. 
-- The result is also a sorted list.
--
-- >>> uniq [1,1,1,1,2,3,3,4,4,4,5,6,6]
-- [1,2,3,4,5,6]
uniq :: Eq a => [a] -> [a]
uniq [] = [] 
uniq (a:as) = uniq' a as
  where
    uniq' u [] = [u]
    uniq' u (a:as)
      | u == a = uniq' u as
      | u /= a = u:uniq' a as 

-- | \(\mathcal{O}(n)\) in the worst case i.e. when the only
-- duplicated elements are at the end of the list.  
-- The 'isDup' function returns true if there are any
-- duplicated values in a sorted list
--
-- >>> uniq [1,1,1,1,2,3,3,4,4,4,5,6,6]
-- True
-- >>> [1,2,3,4,5,6]
-- False
isDup :: Ord a => [a] -> Bool
isDup [] = False
isDup [a] = False
isDup (a1:a2:as)
      | a1 == a2 = True
      | otherwise = isDup (a2:as)

-- | \(\mathcal{O}(n)\). For two sorted lists, 'less'
-- removes from the first, all elements of the second.
lessAll :: Ord a => [a] -> [a] -> [a]
lessAll [] _ = []
lessAll as [] = as
lessAll (a:as) (b:bs)
  | a == b = lessAll as (b:bs)
  | a <  b = a:lessAll as (b:bs)
  | a >  b = lessAll (a:as) bs

-- | \(\mathcal{O}(n)\). For two sorted lists, 'less'
-- removes from the first, the first occurence in the 
-- first list that is the same as an element in the
-- second.
less :: Ord a => [a] -> [a] -> [a]
less [] _ = []
less as [] = as
less (a:as) (b:bs)
  | a == b = less as bs
  | a <  b = a:less as (b:bs)
  | a >  b = less (a:as) bs

-- | \(\mathcal{O}(n)\). Merge two sorted lists and return
-- a sorted list BUT don't take an element from the second
-- list if it is in the first so if there are no duplicates
-- in the first list, the result will not have any 
-- duplicates.
merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs)
  | a == b = a:merge as bs
  | a < b  = a:merge as (b:bs)
  | a > b  = b:merge (a:as) bs 

-- | \(\mathcal{O}(n)\). Merge two sorted lists and return
-- a sorted list AND make sure every element in both lists
-- is included in the correct order.
mergeAll :: Ord a => [a] -> [a] -> [a]
mergeAll [] bs = bs
mergeAll as [] = as
mergeAll (a:as) (b:bs)
  | a <  b = a:mergeAll as (b:bs)
  | a == b = a:b:mergeAll as bs
  | a >  b = b:mergeAll (a:as) bs

-- | \(\mathcal{O}(n)\). Insert an element into a sorted list
-- if it isn't already in the list
insert :: Ord a => [a] -> a -> [a] 
insert as b = merge as [b]

-- | \(\mathcal{O}(n)\). For two sorted lists, return
-- a sorted list that contains the common elements of both
-- Note there will be duplicated values if the first
-- list contains duplicate values.
common :: Ord a => [a] -> [a] -> [a]
common [] bs = []
common as [] = []
common (a:as) (b:bs)
  | a == b = a:common as bs
  | a < b  = common as (b:bs)
  | a > b  = common (a:as) bs 