main :: IO ()
main = do
  print (sumdown 3)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n
  | n > 0 = n + sumdown (n - 1)
