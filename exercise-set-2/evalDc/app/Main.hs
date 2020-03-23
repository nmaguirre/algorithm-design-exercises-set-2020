module Main where

import EvalDc

main :: IO ()
main = do

 

    let x =  (35)

    let z = fibonacciDc x
    
    putStrLn("Use EvalDc")
    -- Call the functions and show results.
    putStrLn ("Input: " ++ (show x))
    putStrLn ("EvalDc for fibonacci function : " ++ (show z))

    