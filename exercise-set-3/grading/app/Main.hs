module Main where

import Data.List

import Grade

main :: IO ()
main = do
    
    let x =  [2, 3, 42, 12, 7]
    let y = Data.List.sort x

     -- Call the functions grade.
    let z = gradeDecrease x  y
    
    let w = grade x

    putStrLn("The longest subsequence between xs: " ++ (show x) )
    putStrLn ("and ys: " ++ (show y))
    putStrLn (" made by decrease function : " ++ (show z))
    putStrLn (" grade is : " ++ (show w))
   