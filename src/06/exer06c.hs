import Prelude hiding (replicate)

main :: IO ()
main = do
  print (replicate 1 1)
  print (replicate 1 'a')

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x
