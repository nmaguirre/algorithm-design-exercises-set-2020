import Test.Hspec

import Test.QuickCheck

import EvalDc

--make sequence of the first n fibonacci numbers
fibList :: Integer -> [Integer]
fibList n = map fibonacciDc [1..n] 

-- sum of the first n fibonacci numbers
sumSeq :: Integer -> Integer
sumSeq n = sum (fibList n)

-- property with the sum of the first n fibonacci numbers
propFib :: Integer -> Property
propFib n   = n>=2  ==> 
                sumSeq n == ( sumSeq (n - 1)  +  sumSeq (n - 2)  + 1)

-- generate Integer between min and max values
bounded_Int :: Integer -> Integer -> Gen Integer
bounded_Int min max = do
    x <- choose (min, max)  :: Gen Integer
    return $ id x


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Fibonacci Divide & Conquer solution" $ do
        it "Test FibonacciDc with 0" $ do
            fibonacciDc 0 `shouldBe` (0 :: Integer)
        it "Test FibonacciDc with 10 " $ do
                fibonacciDc 10 `shouldBe` (55 :: Integer)
        it "Test sum Fibonacci sequence propierty" $ do
                quickCheck (forAll (bounded_Int 0 30) propFib)

        
                  
          

