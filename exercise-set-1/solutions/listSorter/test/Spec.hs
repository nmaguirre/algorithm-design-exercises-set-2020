import Test.Hspec

import qualified Data.List

import ListSorter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe " Specs: sort elements of a list" $ do
        it "Test with empty list selectionSort" $ do
                selectionSort [] `shouldBe` ([] :: [Int])
        it "Test input with ascending order selectionSort" $ do
                selectionSort [1, 2, 5, 44, 333] `shouldBe` ([1,2,5,44,333] :: [Int])
        it "Test input with descending order selectionSort" $ do
                selectionSort [435, 33, 5, 1] `shouldBe` ([1,5,33,435] :: [Int])
        it "Test input with random order selectionSort" $ do
                selectionSort [8, 2, 45, 77, 33, 5, 10] `shouldBe` ([2,5,8,10,33,45,77] :: [Int])
        it "Test input with negative numbers selectionSort" $ do
                selectionSort [-8, -100, -45, -77, -33, -5, -10] `shouldBe` ([-100,-77,-45,-33,-10,-8,-5] :: [Int])
        it "Test with empty list slowSort" $ do
                slowSort [] `shouldBe` ([] :: [Int])
        it "Test input with ascending order slowSort" $ do
                slowSort [1, 2, 5, 44, 333] `shouldBe` ([1,2,5,44,333] :: [Int])                
        it "Test input with random order slowSort" $ do
                slowSort [8, 2, 45, 77, 33, 5, 10] `shouldBe` ([2,5,8,10,33,45,77] :: [Int])
                
        