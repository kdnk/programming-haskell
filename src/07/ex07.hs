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
  print (transmitFake "higher-order functions are easy")

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
parity bits = sum bits `mod` 2

parityBit :: [Bit] -> [Bit]
parityBit [] = [1]
parityBit bits = parity bits : bits

encode :: String -> [Bit]
encode = concat . map (parityBit . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

parityCheck :: [Bit] -> [Bit]
parityCheck [] = []
parityCheck bits =
  if head bits == parity (tail bits)
    then tail bits
    else error "parity check failed."

decode :: [Bit] -> String
decode = map (chr . bin2int . parityCheck) . chop9

transmit :: String -> String
transmit = decode . channel . encode

transmitFake :: String -> String
transmitFake = decode . channelFake . encode

channel :: [Bit] -> [Bit]
channel = id

-- ex 08
channelFake :: [Bit] -> [Bit]
channelFake = tail

