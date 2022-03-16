module ListComprehensions where

concatAll xss = [x | xs <- xss, x <- xs]

firstsAll ps = [x | (x, _) <- ps]

factors upto = [x | x <- [1 .. upto], upto `mod` x == 0]

prime x = factors x == [1, x]

primes n = [x | x <- [2 .. n], prime x]

find k t = [v | (k', v) <- t, k' == k]

pairs xs = zip xs (tail xs)

sorted xs = and [x <= y | (x, y) <- pairs xs]

indexed list = zip list [0 ..]

positions xs value = [index | (value', index) <- indexed xs, value' == value]

lowers string = [x | x <- string, x >= 'a', x <= 'z']