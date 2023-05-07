{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Prelude hiding (dropWhile)

main :: IO ()
main = do
  print (dropWhile (< 3) [1, 2, 3, 4, 5, 1, 2, 3])
  print (dropWhile (< 9) [1, 2, 3])
  print (dropWhile (< 0) [1, 2, 3])

-- print (dropWhile' (< 3) [1, 2, 3, 4, 5, 1, 2, 3])
-- print (dropWhile' (< 9) [1, 2, 3])
-- print (dropWhile' (< 0) [1, 2, 3])

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x : xs)
  | f x = dropWhile f xs
  | otherwise = x : xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f =
  foldr
    ( \x xs ->
        if f x
          then xs
          else x : xs
    )
    []
