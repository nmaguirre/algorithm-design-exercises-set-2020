module ListSorter where

-- Return if a list is order
isOrdered :: Ord a => [a] ->Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:(y :ys)) = if x<=y then isOrdered(y:ys)  else False

-- Remove elemen from list
remove :: (a -> Bool) -> [a] -> [a]
remove p [] = []
remove p (x:xs) = if p x then xs else x : remove p xs


-- Sort a list by selectionSort
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = selectionSort (remove (== x) xs) ++ [x]
  where x = maximum xs

 -- Returns permutations of a list
perms :: Ord a => [a] -> [[a]]
perms [] = [[]]
perms xs = [(x:p)| x<-xs , p <- perms (remove (== x) xs)]

selectOrdered :: Ord a => [[a]] -> [a]
selectOrdered [] = []
selectOrdered (x:xs) = if isOrdered x then x
                else selectOrdered xs

-- Sort a list by Slowsort algorithm
slowSort :: (Ord a) => [a] -> [a]
slowSort xs = selectOrdered (perms xs)


