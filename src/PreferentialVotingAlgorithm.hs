module PreferentialVotingAlgorithm where

import VotingAlgorithms (result)

removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty = filter (/= [])

eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner :: Ord a => [[a]] -> a
winner bs = case rank (removeEmpty bs) of
  [] -> undefined
  [c] -> c
  (c : cs) -> winner (eliminate c bs)