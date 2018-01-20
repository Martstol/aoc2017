module Day5 (solvePart1, solvePart2) where

mapIndex ::Int -> (a -> a) -> [a] -> [a]
mapIndex i f xs = [if i == j then f x else x | (j, x) <- zip [0..] xs]

jump :: Int -> [Int] -> Int
jump i xs = i + (xs !! i)

indexInList :: Int -> [Int] -> Bool
indexInList i xs = i >= 0 && i < length xs

escape :: (Int -> Int) -> [Int] -> Int
escape f xs =
    let iter i n ys = if indexInList i ys
        then iter (jump i ys) (n+1) (mapIndex i f ys)
        else n
    in iter 0 0 xs

solvePart1 :: String -> Int
solvePart1 input =
    let xs = map read (lines input)
    in escape (+1) xs

solvePart2 :: String -> Int
solvePart2 input =
    let xs = map read (lines input)
    in escape (\i -> if i >= 3 then i-1 else i+1) xs