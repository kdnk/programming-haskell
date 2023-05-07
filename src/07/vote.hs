{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.ByteString (sort)
import Data.List qualified as List

{-# HLINT ignore "Avoid lambda using `infix`" #-}
main :: IO ()
main = do
  print (count "Red" votes)
  print (rmdumps votes)
  print (result votes)

  print (rank ballots)

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (\y -> y == x)

rmdumps :: Eq a => [a] -> [a]
rmdumps [] = []
rmdumps (x : xs) = x : rmdumps (filter (\y -> y /= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = List.sort [(count v vs, v) | v <- rmdumps vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- ---------------------------------------------

ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] -> c
  (c : cs) -> winner' (elim c bs)
