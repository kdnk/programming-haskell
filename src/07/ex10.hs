{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

main :: IO ()
main = do
  print (luhn [1, 7, 8, 4])
  print (luhn [4, 7, 8, 3])

  print ()

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- それぞれを独立した番号だとみなす
-- 右から数えて偶数番目の数すべてを二倍にする
-- それぞれの数が9より大きいなら9を引く
-- すべての数を足し合わせる
-- 合計が10で割り切れるなら、カードの番号は正しい

luhnDouble :: Int -> Int
luhnDouble x = if n > 9 then n - 9 else n
  where
    n = x * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0
