{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Prelude hiding (filter, map)

{-# HLINT ignore "Use map" #-}
main :: IO ()
main = do
  print (map (+ 1) [1, 2, 3])
  print (map' (+ 1) [1, 2, 3])

  print (filter odd [1, 2, 3])
  print (filter' odd [1, 2, 3])

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f xs = (f . head) xs : map f (drop 1 xs)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f =
  foldr
    ( \x xs ->
        if f x
          then x : xs
          else xs
    )
    []
