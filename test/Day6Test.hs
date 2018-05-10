module Day6Test where

import           Day6

import           Test.Tasty.Hspec

spec_day6_solvePart1_example :: Spec
spec_day6_solvePart1_example = do
    it "Day 6 part 1 for 0 2 7 0" $
        solvePart1 "0 2 7 0" `shouldBe` 5

spec_day6_solvePart1_input :: Spec
spec_day6_solvePart1_input = do
    input <- runIO (readFile "res/day6.txt")
    it "Day 6 part 1 for input" $
        solvePart1 input `shouldBe` 12841

spec_day6_solvePart2_example :: Spec
spec_day6_solvePart2_example = do
    it "Day 6 part 2 for 0 2 7 0" $
        solvePart2 "0 2 7 0" `shouldBe` 4

spec_day6_solvePart2_input :: Spec
spec_day6_solvePart2_input = do
    input <- runIO (readFile "res/day6.txt")
    it "Day 6 part 2 for input" $
        solvePart2 input `shouldBe` 8038

