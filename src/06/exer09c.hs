{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Prelude hiding (last)

{-# HLINT ignore "Redundant bracket" #-}
main :: IO ()
main = do
  print (last [1, 2])
  print (last [1, 2, 4])

last :: [a] -> a
last [x] = x
last (x : xs) = last xs
