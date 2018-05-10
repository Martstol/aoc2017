module Day5Test where

import           Day5

import           Test.Tasty.Hspec

spec_day5_solvePart1_examples :: Spec
spec_day5_solvePart1_examples = do
    it "Day 5 part 1 for 0\n3\n0\n1\n-3" $
        solvePart1 "0\n3\n0\n1\n-3" `shouldBe` 5

spec_day5_solvePart2_examples :: Spec
spec_day5_solvePart2_examples = do
    it "Day 5 part 2 for 0\n3\n0\n1\n-3" $
        solvePart2 "0\n3\n0\n1\n-3" `shouldBe` 10

spec_day5_solvePart1_input :: Spec
spec_day5_solvePart1_input = do
    input <- runIO (readFile "res/day5.txt")
    it "Day 5 part 1 for input" $
        solvePart1 input `shouldBe` 394829


spec_day5_solvePart2_input :: Spec
spec_day5_solvePart2_input = do
    input <- runIO (readFile "res/day5.txt")
    it "Day 5 part 2 for input" $
        solvePart2 input `shouldBe` 31150702

