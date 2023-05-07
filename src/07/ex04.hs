main :: IO ()
main = do
  print (dec2int [2, 3, 4, 5])
  print (dec2int' [2, 3, 4, 5])

-- 2345
-- = 2 * 1000 + 3 * 100 + 4 * 10 + 5
-- = 2 * 10^3 + 3 * 10^2 + 4 * 10^1 + 5 * 10^0
-- = 10 * (10 * (10 * 2 + 3) + 4) + 5
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

dec2int' :: [Int] -> Int
dec2int' [] = 0
dec2int' (x : xs) = x * (10 ^ length xs) + dec2int' xs
