{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}
import Prelude hiding (concat)

main :: IO ()
main = do
  print (concat [[1, 2], [3]])

concat :: [[a]] -> [a]
concat [[x]] = [x]
concat (xs : ys) = xs ++ concat ys
