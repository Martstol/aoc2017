module Day5Test where

import Day5

import Test.Tasty.Hspec

spec_day5_solvePart1_examples :: Spec
spec_day5_solvePart1_examples = do
    it "Day 5 part 1 for 0\n3\n0\n1\n-3" $
        solvePart1 "0\n3\n0\n1\n-3" `shouldBe` 5

spec_day5_solvePart2_examples :: Spec
spec_day5_solvePart2_examples = do
    it "Day 5 part 2 for 0\n3\n0\n1\n-3" $
        solvePart2 "0\n3\n0\n1\n-3" `shouldBe` 10
