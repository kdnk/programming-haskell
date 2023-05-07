{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Collapse lambdas" #-}
main :: IO ()
main = do
  print ()

uncurry :: (a -> (b -> c)) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y
