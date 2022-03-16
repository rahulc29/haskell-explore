module LuhnAlgorithm where

luhnDouble :: (Ord a, Num a) => a -> a
luhnDouble x
  | x < 5 = 2 * x
  | otherwise = 2 * x - 9

luhn :: Integral a => a -> a -> a -> a -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0