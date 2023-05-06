{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Take on a non-positive" #-}
import Prelude hiding (take)

main :: IO ()
main = do
  print (take 3 [1, 2, 3, 4])
  print (take 3 [1, 2])
  print (take 0 [1, 2])

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x : xs) = x : take (n - 1) xs
