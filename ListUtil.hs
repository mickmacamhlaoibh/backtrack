module ListUtil where

{-
Replace the <i>th element of the list <as> with <a>
-}
replace :: [a] -> Int -> a -> [a]
replace as i a = (take i as) ++ [a] ++ (drop (i+1) as)

{-
Version of foldl' that takes two lists instead of one.
-} 
foldl2' :: (a -> b -> c ->  a) -> a -> [b] -> [c] -> a
foldl2' f a [] _ = a 
foldl2' f a _ [] = a
foldl2' f a (b:bs) (c:cs) = let a' = f a b c 
                            in seq a' $ foldl2' f a' bs cs

{-
Takes a function that will convert a value of type a into a String
and a list of lists of elements of type a and converts the 
list of lists into a String where each individual list is a
line (a String terminated with a newline - \n) and each element 
in each of the lists is converted to a String using the specified 
function and then each element is separated by a space.
-}
toLines :: (a -> String) -> [[a]] -> String
toLines showVal as = unlines $ map (unwords . map showVal) as