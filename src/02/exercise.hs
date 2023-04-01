{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use last" #-}
import Prelude hiding (init, last)

main :: IO ()
main = do
  print (last [1, 2, 3])
  print (last2 [1, 2, 3])
  print (init [1, 2, 3, 4, 5])
  print (init2 [1, 2, 3, 4, 5])

last [n] = n
last (n : ns) = last ns

last2 ns = head (reverse ns)

init [n] = []
init (n : ns) = n : init ns

init2 [n] = []
init2 ns = take (length ns - 1) ns
