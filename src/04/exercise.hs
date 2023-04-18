{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}
main :: IO ()
main = do
  print (halve [1, 2, 3, 4])

  print (third1 [1, 2, 3, 4, 5, 6])
  print (third2 [1, 2, 3, 4, 5, 6])
  print (third3 [1, 2, 3, 4, 5, 6])

  print (safetail1 [1, 2, 3, 4, 5, 6])
  print (safetail2 [1, 2, 3, 4, 5, 6])
  print (safetail3 [1, 2, 3, 4, 5, 6])

  print (safetail1 [])
  print (safetail2 [])
  print (safetail3 [])
  --
  print ()

halve :: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (x : y : z : xs) = z

safetail1 :: Num a => [a] -> [a]
safetail1 xs =
  if null xs
    then []
    else tail xs

safetail2 :: Num a => [a] -> [a]
safetail2 xs
  | null xs = []
  | otherwise = tail xs

safetail3 :: Num a => [a] -> [a]
safetail3 [] = []
safetail3 (x : xs) = xs

-- (||) :: Bool -> Bool -> Bool
-- True || True = True
-- True || False = True
-- False || True = True
-- False || False = False

(||) :: Bool -> Bool -> Bool
False || False = False
True || True = True
True || False = True
False || True = True

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
True |||| _ = True
False |||| b = b
