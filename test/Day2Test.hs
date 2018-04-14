module Day2Test where

import Day2

import Test.Tasty.Hspec

spec_day2_solvePart1_examples :: Spec
spec_day2_solvePart1_examples = do
    it "Day 2 part 1 for example 1" $
        solvePart1 "5 1 9 5\n7 5 3\n2 4 6 8" `shouldBe` 18

spec_day2_solvePart1_input :: Spec
spec_day2_solvePart1_input = do
    input <- runIO (readFile "res/day2.txt")
    it "Day 2 part 1 for input" $
        solvePart1 input `shouldBe` 39126

spec_day2_solvePart2_examples :: Spec
spec_day2_solvePart2_examples = do
    it "Day 2 part 2 for example 1" $
        solvePart2 "5 9 2 8\n9 4 7 3\n3 8 6 5" `shouldBe` 9

spec_day2_solvePart2_input :: Spec
spec_day2_solvePart2_input = do
    input <- runIO (readFile "res/day2.txt")
    it "Day 2 part 2 for input" $
        solvePart2 input `shouldBe` 258