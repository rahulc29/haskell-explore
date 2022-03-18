module ListComprehensionsExercises where

import Data.List (elemIndex)
import Data.Maybe

sumOfSquares :: (Num a, Enum a) => a -> a -> a
sumOfSquares from to = sum [square x | x <- [from .. to]] where square x = x * x

grid :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a, b)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

squareGrid :: (Num b, Enum b) => b -> [(b, b)]
squareGrid n = grid n n

replicate :: (Num t, Enum t) => a -> t -> [a]
replicate element times = [element | _ <- [1 .. times]]

pythagoreanTriplets :: (Num c, Eq c, Enum c) => c -> [(c, c, c)]
pythagoreanTriplets max =
  [ (a, b, c) | a <- [1 .. max], b <- [1 .. max], c <- [1 .. max], a ^ 2 + b ^ 2 == c ^ 2
  ]

factors :: Integral a => a -> [a]
factors n = [d | d <- [1 .. (n - 1)], n `mod` d == 0]

perfects :: Integral a => a -> [a]
perfects n = [x | x <- [1 .. n], isPerfect x] where isPerfect x = sum (factors x) == x

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

positions :: Eq a => [a] -> a -> [Maybe Int]
positions [] value = undefined
positions (x : xs) value = decouple (elemIndex value (x : xs) : [elemIndex value xs])
  where
    decouple maybes = [x | x <- maybes, isJust x]