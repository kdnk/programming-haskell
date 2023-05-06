main :: IO ()
main = do
  print (euclid 6 27)

euclid :: Int -> Int -> Int
euclid x y
  | x > y = euclid (x - y) y
  | x < y = euclid (y - x) x
  | x == y = x
