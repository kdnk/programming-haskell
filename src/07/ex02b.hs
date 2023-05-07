{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
import Prelude hiding (any, takeWhile)

main :: IO ()
main = do
  print (any (\x -> x == 1) [1, 2, 3])
  print (any (\x -> x == 0) [1, 2, 3])

any :: (a -> Bool) -> [a] -> Bool
any f = or . map f
