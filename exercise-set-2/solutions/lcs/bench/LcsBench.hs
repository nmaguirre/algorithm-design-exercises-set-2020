import Criterion.Main
import Control.Monad
import Test.QuickCheck


import qualified Data.List
import Lcs


setupEnv = do
  let xs = [minBound..maxBound] :: [Char]  
  let ys = [minBound..maxBound] :: [Char] 
  return (xs, ys)
    
main :: IO ()
main =  defaultMain  [
    env setupEnv  $ \ ~(xs,ys) -> bgroup "main" [
    bgroup "lcs" [
          bench "lcsDecrease" $ nf (lcsDecrease xs) ys
        , bench "lcsBf" $ nf (lcsBf xs) ys ]
    ] ]