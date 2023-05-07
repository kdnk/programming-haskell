main :: IO ()
main = do
  print ()

ex01 :: (a -> Bool) -> (a -> b) -> [a] -> [b]
ex01 p f xs = map f (filter p xs)
