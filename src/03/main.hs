main :: IO ()
main = do
  print ()

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x * y * z
