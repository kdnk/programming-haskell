main :: IO ()
main = do
  print (altMap (+ 10) (+ 100) [0, 1, 2, 3, 4])
  print (altMap' (+ 10) (+ 100) [0, 1, 2, 3, 4])

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x : xs)
  | even (length xs) = f x : altMap f g xs
  | otherwise = g x : altMap f g xs

altMap' :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap' _ _ [] = []
altMap' f g (x : xs) = f x : altMap' g f xs
