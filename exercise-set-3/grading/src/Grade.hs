module Grade where

import Data.Array
import Data.List

-- grading studient solutions 
grade :: [Int] -> Int
grade xs = length (gradeDecrease (Data.List.sort xs) xs )

gradeDecrease :: Eq a => [a] -> [a] -> [a]
gradeDecrease [] ys = []
gradeDecrease xs [] = []
gradeDecrease (x:xs) (y:ys) 
  | x == y    = x : gradeDecrease xs ys
  | otherwise = longest (gradeDecrease (x:xs) ys) (gradeDecrease xs (y:ys))
    where
        longest xs ys = if length xs > length ys then xs else ys

--grade Optimization
--gradeDP :: Eq a => [a] -> [a] -> [a] || gradeMemo :: Eq a => [a] -> [a] -> [a]
