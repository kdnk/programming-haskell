{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use guards" #-}
main :: IO ()
main = do
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
