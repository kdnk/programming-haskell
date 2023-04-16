{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
main :: IO ()
main = do
  print ((\x -> x + x) 2)
  print ()

even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n

abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n =
  if n < 0
    then -1
    else if n == 0 then 0 else 1

abs2 :: Int -> Int
abs2 n
  | n >= 0 = n
  | otherwise = -n

signum2 :: Int -> Int
signum2 n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

not :: Bool -> Bool
not False = True
not True = False

and :: Bool -> Bool -> Bool
and True True = True
and True False = False
and False True = False
and False False = False

and2 :: Bool -> Bool -> Bool
and2 True True = True
and2 _ _ = False

and3 :: Bool -> Bool -> Bool
and3 True b = True
and3 False _ = False

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, x) = x

test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

test2 :: [Char] -> Bool
test2 ('a' : _) = True
test2 _ = False

head :: [a] -> a
head (x : _) = x

tail :: [a] -> [a]
tail (_ : xs) = xs

add :: Int -> Int -> Int
add x y = x + y

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)

const :: a -> b -> a
const x _ = x

const2 :: a -> (b -> a)
const2 x = (\_ -> x)

(#) = \x -> (\y -> x # y)
(x #) = \y -> x # y
(# y) = \x -> x # y

-- (+) は、加算関数   (\x -> (\y -> x + y))
-- (1+)は、1を加える関数 (\y -> 1 + y)
-- (1/)は、逆数を取る関数 (\y -> 1/y)
-- (*2)は倍にす関数 (\y -> y * 2)
-- (/2)は、半部にする関数 (\y -> y / 2)

sum :: [Int] -> Int
sum = foldl (+) 0

