import Data.Char

main :: IO ()
main = do
  print (let2int 'a')
  print (int2let 0)

  print (shift 3 'a')
  print (shift 3 'z')
  print (shift (-3) 'c')
  print (shift 3 ' ')

  print (encode 3 "haskell is fun")
  print (encode (-3) "kdvnhoo lv ixq")

  print (percent 5 15)

  print (freqs "abbcccddddeeeee")

  print (rotate 3 [1, 2, 3, 4, 5])

  print table

  print (crack "kdvnhoo lv ixq")

  print ()

-- 小文字のみを暗号化

-- a -> 0, b -> 1, ... , z -> 25
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

lowers :: String -> Int
lowers xs = length [x | x <- xs, 'a' <= x && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- 任意の文字に対して、出現頻度表を返す関数
freqs :: String -> [Float]
freqs xs = [percent (count x xs) (lowers xs) | x <- ['a' .. 'z']]

-- カイ二乗検定
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = after ++ before
  where
    (before, after) = splitAt n xs

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x' == x]

crack :: String -> String
crack xs = encode (-factor) xs
  where
    table' = freqs xs
    chitable = [chisqr (rotate n table') table | n <- [0 .. 25]]
    factor = head (positions (minimum chitable) chitable)
