module Day2 (solvePart1, solvePart2) where

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

solvePart1 :: String -> Int
solvePart1 input = checksum minMaxDiff (parseInput input)

minMaxDiff :: [Int] -> Int
minMaxDiff xs = maximum xs - minimum xs

solvePart2 :: String -> Int
solvePart2 input = checksum evenlyDivisible (parseInput input)

evenlyDivisible :: [Int] -> Int
evenlyDivisible xs = head [div x y | x <- xs, y <- xs, x /= y && (rem x y == 0)]

checksum :: ([Int] -> Int) -> [[Int]] -> Int
checksum f xs = sum (map f xs)

