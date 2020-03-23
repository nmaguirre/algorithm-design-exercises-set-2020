module EvalDc where

-- evalDc a higher-order function to construct D&C solutions
evalDc :: Eq a => (a -> Bool) -> (a -> b) -> (a -> [a]) -> ([b] -> b) -> a -> b
evalDc isBase base split join x | isBase x = base x
                              | otherwise = join (map (evalDc isBase base split join) (split x))


-- fibonacci function using evalDc
fibonacciDc :: Integer -> Integer
fibonacciDc = evalDc (\x->x<=1) (\x ->x) (\x->[x-1,x-2]) (\l->(head l)+(last l))

-- Given a pair of sequences p and t, decide whether p is subsequence (of contiguous elements) of t.
-- subsequ :: Eq a => (String,String) -> Bool
-- Not yet implemented