main :: IO ()
main = do
  print (merge [2, 5, 6] [1, 3, 4])

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x >= y = y : merge (x : xs) ys
