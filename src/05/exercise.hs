import Prelude hiding (replicate)

main :: IO ()
main = do
  print sigma

  print (grid 1 2)

  print (square 2)

  print (replicate 3 True)

  print (pyths 10)

  print (factors 10)

  print (perfects 500)

  print list2

  print (positions False [True, False, True, False])

-- 1
-- 1 から 100までの二乗和
sigma :: Int
sigma = sum [n ^ 2 | n <- [0 .. 100]]

-- 2
-- m×nの座標格子が、0 <= x <= m, 0 <= y <= nに対し、
-- すべての整数の組 (x,y)で表現されている。
-- 与えられた大きさの座標格子を返す関数 grid
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

-- 3
-- リスト内包表記１つとgridを用いて、
-- 大きさ n の正方形座標を返す関数 square を定義
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4
-- replicate を、内包表記を用いて定義
replicate :: Int -> a -> [a]
replicate n x = [x | i <- [1 .. n]]

-- 5
-- ピタゴラス数 (x^2 + y^2 = z^2 を満たす、正の整数) のリストを生成する関数 pyths :: Int -> [(Int, Int, Int)]
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [0 .. n], y <- [0 .. n], z <- [0 .. n], x < z, y < z, (x ^ 2) + y ^ 2 == z ^ 2]

-- 6
-- 自分自身を除く約数の和が自分自身と等しいとき、その整数を完全数と呼ぶ
-- 与えられた上限までに含まれる完全数すべてを算出する関数 perfects をリスト内包表記と factors を使って定義
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (factors x) - x == x]

-- 7
-- ２つの生成器をもつリスト内包表記 [(x,y) | x <- [1,2,3], y <- [4,5,6]] は
-- １つの生成器を持つリスト内包表記２つでも表現できることを示してください。
-- ヒント: 一方のリスト内包表記を他方の中に入れ、プレリュード関数 concat を使う
list2 :: [(Int, Int)]
list2 = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- 8
-- positions を find を使って再定義
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 ..])

-- 9
-- 長さが n である整数のリスト xs と ys の内積は、対応する要素の積の和として計算できる。
-- ２つのリストから内積を計算する関数 scalarproduct を、
-- 関数 chisqr と同じように、リスト内包表記を使って定義
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
