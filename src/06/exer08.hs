{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}
main :: IO ()
main = do
  print (merge [2, 5, 6] [1, 3, 4])
  print (halve [2, 5, 6, 1, 3, 4])
  print (halve [2, 5, 6, 1, 3, 4, 7])
  print (halve [2, 5, 6, 1, 3, 4, 7, 8])
  print (msort [2, 5, 6, 1, 3, 4, 7, 8])

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x >= y = y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort firsts) (msort seconds)
  where
    (firsts, seconds) = halve xs

-- msort (x:xs) =
