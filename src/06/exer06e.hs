import Prelude hiding (elem)

main :: IO ()
main = do
  print (1 `elem` [1, 2, 3])
  print (4 `elem` [1, 2, 3])

elem :: Eq a => a -> [a] -> Bool
elem n [] = False
elem n (x : xs)
  | n == x = True
  | otherwise = elem n xs
