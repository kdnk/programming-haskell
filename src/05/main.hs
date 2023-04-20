import Prelude hiding (length)

main :: IO ()
main = do
  print (factors 15)
  print (factors 7)

  print (factors 7)

  print (prime 10)
  print (prime 1)
  print (prime 2)
  print (prime 3)

  print ()

  print (prime2 10)
  print (prime2 1)
  print (prime2 2)
  print (prime2 3)

  print ()

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts xs = [x | (x, _) <- xs]

length :: [a] -> Int
length xs = sum [1 | _ <- xs]

filterEven :: Integral a => [a] -> [a]
filterEven xs = [x | x <- xs, even x]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

prime :: Int -> Bool
prime n
  | n == 1 = False
  | n >= 2 = length [x | x <- [2 .. n], n `mod` x == 0] == 1

prime2 :: Int -> Bool
prime2 n = factors n == [1, n]
