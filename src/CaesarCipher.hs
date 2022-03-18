module CaesarCipher where

import Data.Char

toInt :: Char -> Int
toInt char = ord char - ord 'a'

toChar :: Int -> Char
toChar int = chr (int + ord 'a')

shift :: Char -> Int -> Char
shift x s
  | isLower x = toChar ((toInt x + s) `mod` 26)
  | otherwise = x

encode :: [Char] -> Int -> [Char]
encode string factor = [shift x factor | x <- string]

decode :: [Char] -> Int -> [Char]
decode string factor = encode string (- factor)

--- Table of frequencies for letters in English
table :: [Float]
table =
  [ 8.1,
    1.5,
    2.8,
    4.2,
    12.7,
    2.2,
    2.0,
    6.1,
    7.0,
    0.2,
    0.8,
    4.0,
    2.4,
    6.7,
    7.5,
    1.9,
    0.1,
    6.0,
    6.3,
    9.0,
    2.8,
    1.0,
    2.4,
    0.2,
    2.0,
    0.1
  ]

fractionAsPercent :: (Fractional a1, Integral a2, Integral a3) => a2 -> a3 -> a1
fractionAsPercent a b = (fromIntegral a / fromIntegral b) * 100

count :: Eq a => a -> [a] -> Int
count x xs = length [y | y <- xs, y == x]

lowers :: [Char] -> Int
lowers chs = length [x | x <- chs, isLower x]

freqs :: Fractional a => [Char] -> [a]
freqs xs = [fractionAsPercent (count x xs) n | x <- ['a' .. 'z']] where n = lowers xs

chiSquare :: Fractional a => a -> a -> a
chiSquare observed expected = ((observed - expected) ^ 2) / expected

compareFrequencyTables :: Fractional a => [a] -> [a] -> a
compareFrequencyTables observed expected = sum [chiSquare obs exp | (obs, exp) <- zip observed expected]

rotateLeft :: [a] -> Int -> [a]
rotateLeft list n = drop n list ++ take n list

frequencyTablesChi :: [Char] -> [Float]
frequencyTablesChi string = [compareFrequencyTables (rotateLeft table' n) table | n <- [0 .. 25]] where table' = freqs string

indexed :: (Num b, Enum b) => [a] -> [(a, b)]
indexed list = zip list [0 ..]

positions :: (Num a1, Enum a1, Eq a2) => [a2] -> a2 -> [a1]
positions xs value = [index | (value', index) <- indexed xs, value' == value]

crackCaesar :: [Char] -> [Char]
crackCaesar string = decode string factor
  where
    factor = head (positions chiTable (minimum chiTable))
    chiTable = frequencyTablesChi string