module ListComprehensions where

concatAll :: [[a]] -> [a]
concatAll xss = [x | xs <- xss, x <- xs]

firstsAll :: [(a, b)] -> [a]
firstsAll ps = [x | (x, _) <- ps]

factors :: Integral a => a -> [a]
factors upto = [x | x <- [1 .. upto], upto `mod` x == 0]

prime :: Integral a => a -> Bool
prime x = factors x == [1, x]

primes :: Integral a => a -> [a]
primes n = [x | x <- [2 .. n], prime x]

find :: Eq a1 => a1 -> [(a1, a2)] -> [a2]
find k t = [v | (k', v) <- t, k' == k]

pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

indexed :: (Num b, Enum b) => [a] -> [(a, b)]
indexed list = zip list [0 ..]

positions :: (Num a1, Enum a1, Eq a2) => [a2] -> a2 -> [a1]
positions xs value = [index | (value', index) <- indexed xs, value' == value]

lowers :: [Char] -> [Char]
lowers string = [x | x <- string, x >= 'a', x <= 'z']

count :: Eq a => a -> [a] -> Int
count char string = length [x | x <- string, x == char]
--- count 'a' "rahul chhabra" = 3