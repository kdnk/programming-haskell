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
