module Main where

import Day1
import Day2
import Day3
import Day4

getInputForDay :: Int -> String
getInputForDay n = "res/day" ++ show n ++ ".txt"

main :: IO ()
main = do
    content <- readFile (getInputForDay 4)
    print (Day4.solvePart2 content)
