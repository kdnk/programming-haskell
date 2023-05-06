{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (sum)

main :: IO ()
main = do
  print (sum [1, 2, 3])

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs
