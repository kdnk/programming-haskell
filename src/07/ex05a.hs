{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
main :: IO ()
main = do
  print ()

curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f = \x -> (\y -> f (x, y))
