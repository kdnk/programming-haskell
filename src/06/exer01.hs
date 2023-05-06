{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (product)

main :: IO ()
main = do
  print (fac (-1))

fac :: Int -> Int
fac 0 = 1
fac n
  | n > 0 = n * fac (n - 1)
