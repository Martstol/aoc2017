module Day5 (solvePart1, solvePart2) where

import Data.Array.Unboxed

follow :: (Int -> Int) -> UArray Int Int -> Int -> Int -> Int
follow f x i acc
    | inRange (bounds x) i = follow f (x // [(i, f (x ! i))]) (i + (x ! i)) (acc+1)
    | otherwise            = acc

parseInstructions :: String -> UArray Int Int
parseInstructions input =
    let arrayContent = map read (lines input)
        arrayBounds  = (0, length arrayContent - 1)
    in listArray arrayBounds arrayContent

solvePart1 :: String -> Int
solvePart1 input =
    let instructions = parseInstructions input
    in follow (+1) instructions 0 0

solvePart2 :: String -> Int
solvePart2 input =
    let instructions = parseInstructions input
        modify       = \i -> if i >= 3 then i-1 else i+1
    in follow modify instructions 0 0