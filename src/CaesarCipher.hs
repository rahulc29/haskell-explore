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

decode string factor = encode string (- factor)