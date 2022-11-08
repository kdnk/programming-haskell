{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Prelude hiding (product)

{-# HLINT ignore "Use foldr" #-}
main :: IO ()
main = do
  -- 2.
  print (product [2, 3, 4])
  print (qsort [2, 2, 3, 1, 1])

product [] = 1
product (x : xs) = x * product xs

qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, x < b]
