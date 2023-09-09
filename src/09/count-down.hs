import Data.Bits (Bits (xor))

main :: IO ()
main = do
  print ()
  print (values (App Add (Val 1) (App Mul (Val 2) (Val 3))))
  print (eval (App Add (Val 1) (App Mul (Val 2) (Val 3))))
  print (eval (App Add (Val 2) (App Mul (Val 2) (Val 3))))
  print (eval (App Add (Val 2) (App Div (Val 3) (Val 3))))
  print (eval (App Add (Val 2) (App Div (Val 3) (Val 2))))
  print (interleave 1 [2, 3, 4])
  print (perms [1, 2, 3])

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = subs xs ++ map (x :) (subs xs)

interleave :: a -> [a] -> [[a]]
interleave n [] = [[n]]
interleave n (x : xs) = (n : x : xs) : map (x :) (interleave n xs)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices [] = [[]]
choices xs = concat (map perms (subs xs))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]
