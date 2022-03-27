module VotingAlgorithms where

import Data.List (sort)

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : filter (/= x) (removeDuplicates xs)

result :: Ord b => [b] -> [(Int, b)]
result votes = sort [(count x votes, x) | x <- removeDuplicates votes]

winner :: Ord a => [a] -> a
winner = snd . last . result