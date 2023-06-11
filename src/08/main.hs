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
