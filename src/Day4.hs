module Day4 (solvePart1, solvePart2) where

import           Data.List (nub, sort)

noDuplicates :: Eq a => [a] -> Bool
noDuplicates x = nub x == x

validPassphrase :: String -> Bool
validPassphrase phrase = noDuplicates (words phrase)

numberOfValidPassphrases :: [String] -> Int
numberOfValidPassphrases phrases = length (filter validPassphrase phrases)

solvePart1 :: String -> Int
solvePart1 input = numberOfValidPassphrases (lines input)

isAnagram :: String -> String -> Bool
isAnagram a b = sort a == sort b

anagrams :: [String] -> [String]
anagrams w = [a | a <- w, b <- w, a /= b && isAnagram a b]

noAnagrams :: [String] -> Bool
noAnagrams x = null (anagrams x)

validPassphrase' :: String -> Bool
validPassphrase' phrase = let
        w = words phrase
    in noDuplicates w && noAnagrams w

numberOfValidPassphrases' :: [String] -> Int
numberOfValidPassphrases' phrases = length (filter validPassphrase' phrases)

solvePart2 :: String -> Int
solvePart2 input = numberOfValidPassphrases' (lines input)

