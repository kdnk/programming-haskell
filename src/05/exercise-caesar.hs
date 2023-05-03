import Data.Char

main :: IO ()
main = do
  print (ord 'A')
  print (ord 'Z')
  print (ord 'a')
  print (ord 'z')

  print (crack "kdvnhoo lv ixq")
  print (crack "Kdvnhoo lv ixq")
  print ()

-- 小文字と大文字を暗号化

-- a -> 0, b -> 1, ... , z -> 25
let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | otherwise = ord c - ord 'A'

int2letLower :: Int -> Char
int2letLower n = chr (ord 'a' + n)

int2letUpper :: Int -> Char
int2letUpper n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2letLower ((let2int c + n) `mod` 26)
  | isUpper c = int2letUpper ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

lowers :: String -> Int
lowers xs = length [x | x <- xs, 'a' <= x && x <= 'z']

alphabetsLength :: String -> Int
alphabetsLength xs = length [x | x <- xs, ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')]

count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x == x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- 任意の文字に対して、出現頻度表を返す関数
freqs :: String -> [Float]
freqs xs = [percent (count l xs + count u xs) (alphabetsLength xs) | (l, u) <- zip ['a' .. 'z'] ['A' .. 'Z']]

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
