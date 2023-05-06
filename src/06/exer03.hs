import Prelude hiding ((^))

main :: IO ()
main = do
  print (2 ^ 3)

(^) :: Int -> Int -> Int
_ ^ 0 = 1
x ^ y | y > 0 = x * (x ^ (y - 1))
