module Day3 (solvePart1) where

import Data.Complex

createMovementList :: [Complex Double] -> [Int] -> [Complex Double]
createMovementList (fst:snd:tail) (x:xs) = (replicate x fst) ++ (replicate x snd) ++ (createMovementList tail xs)

movementList :: [Complex Double]
movementList = createMovementList (iterate (*(0 :+ 1)) (1 :+ 0)) [1..]

positions :: [Complex Double]
positions = scanl (+) (0 :+ 0) movementList

manhattenDistance :: Complex Double -> Int
manhattenDistance c = round ((abs (realPart c)) + (abs (imagPart c)))

solvePart1 :: String -> Int
solvePart1 input = let
        n = read input
    in manhattenDistance (positions !! (n-1))