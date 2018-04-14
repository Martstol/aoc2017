module Day3Test where

import Day3

import Test.Tasty.Hspec

spec_day3_solvePart1_examples :: Spec
spec_day3_solvePart1_examples = do
    it "Day 3 part 1 for 1" $
        solvePart1 "1" `shouldBe` 0
    it "Day 3 part 1 for 12" $
        solvePart1 "12" `shouldBe` 3
    it "Day 3 part 1 for 23" $
        solvePart1 "23" `shouldBe` 2
    it "Day 3 part 1 for 1024" $
        solvePart1 "1024" `shouldBe` 31

spec_day3_solvePart1_input :: Spec
spec_day3_solvePart1_input = do
    input <- runIO (readFile "res/day3.txt")
    it "Day 3 part 1 for input" $
        solvePart1 input `shouldBe` 552

spec_day3_solvePart2_input :: Spec
spec_day3_solvePart2_input = do
    input <- runIO (readFile "res/day3.txt")
    it "Day 3 part 2 for input" $
        solvePart2 input `shouldBe` 330785