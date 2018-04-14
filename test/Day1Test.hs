module Day1Test where

import Day1

import Test.Tasty.Hspec

spec_day1_solvePart1_examples :: Spec
spec_day1_solvePart1_examples = do
    it "Day 1 part 1 for 1122" $
        solvePart1 "1122" `shouldBe` 3
    it "Day 1 part 1 for 1111" $
        solvePart1 "1111" `shouldBe` 4
    it "Day 1 part 1 for 1234" $
        solvePart1 "1234" `shouldBe` 0
    it "Day 1 part 1 for 91212129" $
        solvePart1 "91212129" `shouldBe` 9

spec_day1_solvePart2_examples :: Spec
spec_day1_solvePart2_examples = do
    it "Day 1 part 2 for 1212" $
        solvePart2 "1212" `shouldBe` 6
    it "Day 1 part 2 for 1221" $
        solvePart2 "1221" `shouldBe` 0
    it "Day 1 part 2 for 123425" $
        solvePart2 "123425" `shouldBe` 4
    it "Day 1 part 2 for 123123" $
        solvePart2 "123123" `shouldBe` 12
    it "Day 1 part 2 for 12131415" $
        solvePart2 "12131415" `shouldBe` 4

spec_day1_solvePart1_input :: Spec
spec_day1_solvePart1_input = do
    input <- runIO (readFile "res/day1.txt")
    it "Day 1 part 1 for input" $
        solvePart1 input `shouldBe` 1158

spec_day1_solvePart2_input :: Spec
spec_day1_solvePart2_input = do
    input <- runIO (readFile "res/day1.txt")
    it "Day 1 part 2 for input" $
        solvePart2 input `shouldBe` 1132