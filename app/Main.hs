module Main where

import Day1 (solvePart1, solvePart2)
import Day2 (solvePart1, solvePart2)
import Day3 (solvePart1)

getInputForDay :: Int -> String
getInputForDay n = "res/day" ++ (show n) ++ ".txt"

main :: IO ()
main = do
    content <- readFile (getInputForDay 3)
    print (Day3.solvePart1 content)
