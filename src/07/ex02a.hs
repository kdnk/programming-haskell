{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use all" #-}

import Prelude hiding (all)

{-# HLINT ignore "Use any" #-}
main :: IO ()
main = do
  print (all (\x -> x == 1) [1, 2, 3])
  print (all (\x -> x /= 4) [1, 2, 3])

all :: (a -> Bool) -> [a] -> Bool
all f = null . filter not . map f

all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f
