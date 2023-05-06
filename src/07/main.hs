{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}

import Prelude hiding (filter, length, map, reverse, sum)

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
main :: IO ()
main = do
  print (map (1 +) [1, 3, 5, 7])
  print (map (+ 1) [1, 3, 5, 7])
  print (filter even [1, 2, 3, 4])

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

sumsqreven :: [Int] -> Int
sumsqreven xs = sum (map (^ 2) (filter even xs))

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True

sum' xs = foldr (+) 0 xs

length :: [a] -> Int
length [] = 0
length (x : xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr snoc []

sum'' :: [Int] -> Int
sum'' = sum''' 0
  where
    sum''' v [] = v
    sum''' v (x : xs) = sum''' (v + x) xs
