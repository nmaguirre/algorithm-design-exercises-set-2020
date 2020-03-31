module Main where

import Lcs

main :: IO ()
main = do
    
    let x =  [2, 3, 42, 12, 7]
    let y =  [2, 5, 12, 42, 7]

     -- Call the functions lcs.

    let z = lcsDecrease x y 
    putStrLn("The longest common subsequence between xs: " ++ (show x) )
    putStrLn ("and ys: " ++ (show y))
    putStrLn ("lcs by lcsDecrease function : " ++ (show z))

    --let s = subseq x
    --putStrLn("The subsequence of xs: " ++ (show s) )

