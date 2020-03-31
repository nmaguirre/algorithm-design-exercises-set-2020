module Lcs where

import Data.List
import Data.Function


--get subsequences
subseq :: [a] -> [[a]]
subseq xs = sortBy (compare `on` length) $ subsequences xs

-- longest common subsequence by brute force
lcsBf :: Eq a => [a] -> [a] -> [a]
lcsBf xs ys = maximumBy (compare `on` length)(filter (\x -> elem x (subseq ys)) (subseq xs))

-- longest common subsequence by decrease and conquer 
lcsDecrease :: Eq a => [a] -> [a] -> [a]
lcsDecrease [] ys = []
lcsDecrease xs [] = []
lcsDecrease (x:xs) (y:ys) 
  | x == y = x : (lcsDecrease xs ys)
  | otherwise = longest (lcsDecrease xs (y:ys)) (lcsDecrease (x:xs) ys)
    where
      longest xs ys = if length xs > length ys then xs else ys



  