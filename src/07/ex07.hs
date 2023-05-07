{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

import Data.Bits (Bits (xor))
import Data.Char

type Bit = Int

main :: IO ()
main = do
  -- print (bin2int [1, 0, 1, 1])
  -- print (int2bin 13)
  -- print (make8 [1, 0, 1, 1])
  --
  -- print (encode "abc")
  -- print (decode [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0])

  print (transmit "higher-order functions are easy")

  print ()

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Int
parity xs = length (filter (\x -> x == 1) xs)

parityBit :: [Bit] -> [Bit]
parityBit [] = [1]
parityBit xs
  | odd (parity xs) = 1 : xs
  | otherwise = 0 : xs

encode :: String -> [Bit]
encode = concat . map (make8 . parityBit . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

parityCheck :: [Bit] -> [Bit]
parityCheck [] = []
parityCheck xs =
  if head xs == parity (tail xs)
    then tail xs
    else error "parity check failed."

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8 . parityCheck

transmit :: String -> String
transmit = decode . fakeChannel . encode

channel :: [Bit] -> [Bit]
channel = id

-- ex 08
fakeChannel :: [Bit] -> [Bit]
fakeChannel = tail

