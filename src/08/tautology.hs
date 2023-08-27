main :: IO ()
main = do
  print ()

data Prop
  = And Prop Prop
  | Not Prop
  | Imply Prop Prop
  | Var Char
  | Const Bool

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (True :) (bools (n - 1)) ++ map (False :) (bools (n - 1))

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : filter (/= x) xs

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)
