import Prelude hiding (Just, Maybe, Nothing)

main :: IO ()
main = do
  print ()

type Pos = (Int, Int)

type Trans = Pos -> Pos

type Pair a = (a, a)

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv a b = Just (a `div` b)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2ant :: Int -> Nat
int2ant 0 = Zero
int2ant n = Succ (int2ant (n - 1))

add :: Nat -> Nat -> Nat
add m n = int2ant (nat2int m + nat2int n)

add2 :: Nat -> Nat -> Nat
add2 Zero n = n
add2 (Succ m) n = Succ (add2 m n)

-- list
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node t1 y t2)
  | x == y = True
  | otherwise = occurs x t1 || occurs x t2

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node t1 x t2) = flatten t1 ++ [x] ++ flatten t2

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y) = x == y
occurs2 x (Node t1 y t2)
  | x == y = True
  | y < x = occurs2 x t2
  | y > x = occurs2 x t1
