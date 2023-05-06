import Prelude hiding ((!!))

main :: IO ()
main = do
  print ([1, 2, 3] !! 1)

(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(x : xs) !! n = xs !! (n - 1)
