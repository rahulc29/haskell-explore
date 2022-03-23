module Recursion where

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

insert :: Ord t => t -> [t] -> [t]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop 0 list = list
myDrop _ [] = []
myDrop n (x : xs) = myDrop (n - 1) xs

factorial :: (Ord t, Integral t) => t -> t
factorial n
  | n < 0 = undefined
  | n < 2 = n
  | otherwise = n * (factorial n - 1)

sumdown :: (Eq p, Num p) => p -> p
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

euclid :: Integral t => t -> t -> t
euclid a b = if b == 0 then a else euclid b (b `mod` a)

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = if x then myAnd xs else False

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ (myConcat xs)

myReplicate :: (Eq t, Num t) => t -> a -> [a]
myReplicate 0 elem = []
myReplicate n elem = elem : myReplicate (n - 1) elem

elementAt :: (Eq t, Num t) => [p] -> t -> p
elementAt [] _ = undefined
elementAt (x : xs) n = if n == 0 then x else elementAt xs (n - 1)

elementExists :: Eq t => t -> [t] -> Bool
elementExists elem [] = False
elementExists elem (x : xs) = if x == elem then True else elementExists elem xs

--- Custom operator for list mergers
(<+>) :: Ord a => [a] -> [a] -> [a]
(<+>) [] [] = []
(<+>) xs [] = xs
(<+>) [] ys = ys
(<+>) (x : xs) (y : ys) = if x <= y then x : (<+>) xs (y : ys) else y : (<+>) (x : xs) ys

mergeSort [] = []
mergeSort [x] = [x]
mergeSort [x, y] = [x] <+> [y]
mergeSort list = mergeSort left <+> mergeSort right
  where
    (left, right) = halve list
      where
        halve list = splitAt (n `div` 2) list
          where
            n = length list