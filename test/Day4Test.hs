module Day4Test where

import Day4

import Test.Tasty.Hspec

spec_day4_solvePart1_examples :: Spec
spec_day4_solvePart1_examples = do
    it "Day 4 part 1 for example 1" $
        solvePart1 "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa\n" `shouldBe` 2

spec_day4_solvePart2_examples :: Spec
spec_day4_solvePart2_examples = do
    it "Day 4 part 2 for example 1" $
        solvePart2 "abcde fghij\nabcde xyz ecdab\na ab abc abd abf abj\niiii oiii ooii oooi oooo\noiii ioii iioi iiio\n" `shouldBe` 3

spec_day4_solvePart1_input :: Spec
spec_day4_solvePart1_input = do
    input <- runIO (readFile "res/day4.txt")
    it "Day 4 part 1 for input" $
        solvePart1 input `shouldBe` 337

spec_day4_solvePart2_input :: Spec
spec_day4_solvePart2_input = do
    input <- runIO (readFile "res/day4.txt")
    it "Day 4 part 2 for input" $
        solvePart2 input `shouldBe` 231