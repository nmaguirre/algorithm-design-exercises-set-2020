import Criterion.Main
import EvalDc


main :: IO ()
main = defaultMain [
                bgroup "fib" [ bench "fib 10" $ whnf fibonacciDc 10
                     , bench "fib 35" $ whnf fibonacciDc 35
                     , bench "fib 37" $ whnf fibonacciDc 37
                ]
            ]


