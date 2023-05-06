{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use &&" #-}
import Prelude hiding (and)

main :: IO ()
main = do
  print (and [True, True])
  print (and [])
  print (and [False, True])

and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs
