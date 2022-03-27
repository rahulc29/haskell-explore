module BinaryStringTransmitter where

import Data.Char (chr, ord)

type Bit = Int

bitsToIntComp :: [Bit] -> Int
bitsToIntComp bits = sum [w * b | (w, b) <- zip weights bits] where weights = iterate (* 2) 1

bitsToIntFolded :: [Bit] -> Int
bitsToIntFolded = foldr (\x y -> x + 2 * y) 0

toBits :: Int -> [Bit]
toBits 0 = []
toBits n = (n `mod` 2) : toBits (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: [Char] -> [Bit]
encode = concatMap (make8 . toBits . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 stream = take 8 stream : chop8 (drop 8 stream)

decode :: [Bit] -> [Char]
decode = map (chr . bitsToIntFolded) . chop8

transmit :: [Char] -> [Char]
transmit = decode.channel.encode

channel :: a -> a
channel = id