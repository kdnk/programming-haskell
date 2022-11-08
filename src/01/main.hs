{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (sum)

main :: IO ()
main = do
  print (sum [0, 1])
  print (qsort [2, 2, 3, 1, 1])

sum :: Num a => [a] -> a
sum [] = 0
sum (n : ns) = n + sum ns

qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, x < b]
