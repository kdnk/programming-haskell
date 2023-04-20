{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Redundant lambda" #-}
main :: IO ()
main = do
  print (halve [1, 2, 3, 4])

  print (third1 [1, 2, 3, 4, 5, 6])
  print (third2 [1, 2, 3, 4, 5, 6])
  print (third3 [1, 2, 3, 4, 5, 6])

  print (safetail1 [1, 2, 3, 4, 5, 6])
  print (safetail2 [1, 2, 3, 4, 5, 6])
  print (safetail3 [1, 2, 3, 4, 5, 6])

  print (safetail1 [])
  print (safetail2 [])
  print (safetail3 [])

  print (luhnDouble 3)
  print (luhnDouble 6)

  print (luhn 1 7 8 4)
  print (luhn 4 7 8 3)
  --
  print ()

halve :: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (x : y : z : xs) = z

safetail1 :: Num a => [a] -> [a]
safetail1 xs =
  if null xs
    then []
    else tail xs

safetail2 :: Num a => [a] -> [a]
safetail2 xs
  | null xs = []
  | otherwise = tail xs

safetail3 :: Num a => [a] -> [a]
safetail3 [] = []
safetail3 (x : xs) = xs

-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False

-- 4
(||) :: Bool -> Bool -> Bool
False || False = False
True || True = True
True || False = True
False || True = True

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
True |||| _ = True
False |||| b = b

-- 5
(&&) :: Bool -> Bool -> Bool
a && b =
  if a == True
    then
      if b == True
        then True
        else False
    else False

-- 6
(&&&) :: Bool -> Bool -> Bool
a &&& b =
  if a == True
    then b
    else False

-- 7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- 8
-- それぞれを独立した番号だとみなす
-- 右から数えて偶数番目の数すべてを二倍にする
-- それぞれの数が9より大きいなら9を引く
-- すべての数を足し合わせる
-- 合計が10で割り切れるなら、カードの番号は正しい

luhnDouble :: Int -> Int
luhnDouble x = if n > 9 then n - 9 else n
  where
    n = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = s `mod` 10 == 0
  where
    s = luhnDouble a + b + luhnDouble c + d
