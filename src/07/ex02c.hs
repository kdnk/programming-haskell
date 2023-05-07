{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Prelude hiding (any, takeWhile)

main :: IO ()
main = do
  print (takeWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4])
  print (takeWhile (< 9) [1, 2, 3])
  print (takeWhile (< 0) [1, 2, 3])

-- print (takeWhile' (< 3) [1, 2, 3, 4, 1, 2, 3, 4])

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x : xs)
  | f x = x : takeWhile f xs
  | otherwise = []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f =
  foldr
    ( \x xs ->
        if f x
          then x : xs
          else []
    )
    []
