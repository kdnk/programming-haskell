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
  print (primes 40)

  print ()
  print (find 'b' [('a', 1), ('b', 2), ('c', 3), ('b', 4)])

  print (zip ['a', 'b', 'c'] [1, 2, 3, 4])

  print (pairs [1, 2, 3, 4])
  print (sorted [1, 2, 3, 4])
  print (sorted [1, 3, 2, 4])

  print (positions False [True, False, True, False])

  print (lowers "hellO")
  print (lowers "hello")
  print (count 'h' "hello")
  print (count 'l' "hello")

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

prime :: Int -> Bool
prime n
  | n == 1 = False
  | n >= 2 = length [x | x <- [2 .. n], n `mod` x == 0] == 1

prime2 :: Int -> Bool
prime2 n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x' == x]

lowers :: String -> Int
lowers xs = length [x | x <- xs, 'a' <= x && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x == x']
