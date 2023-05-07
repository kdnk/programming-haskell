{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Eta reduce" #-}
import Prelude hiding (iterate, map)

main :: IO ()
main = do
  print (int2bin 13)
  print (chop8 [1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0])
  print (map' (+ 1) [1, 2, 3])
  print (map (+ 1) [1, 2, 3])

  print (take 10 $ iterate' (+ 3) 42)
  print (take 10 $ iterate (+ 3) 42)
  print ()

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f xs = (f . head) xs : map f (drop 1 xs)

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) (drop 1)

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\_ -> False) id f
