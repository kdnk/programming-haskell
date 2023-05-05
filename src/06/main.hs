{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (drop, length, product, reverse, sum, zip, (++))

main :: IO ()
main = do
  print ()

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m * n = m + (m * (n - 1))

product :: Num a => [a] -> a
product [] = 1
product (n : ns) = n * product ns

length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x > y = y : insert x ys
  | otherwise = x : y : ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs
