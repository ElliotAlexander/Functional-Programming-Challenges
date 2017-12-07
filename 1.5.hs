
data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 5
substitute :: Expr -> Int -> Expr -> Expr
substitute (Lam x e1) i e2 | x == i = (App e2 (substitute e1 i e2)) | otherwise = (Lam x (substitute e1 i e2))
substitute (App e1 e2) i e3 = (App (substitute e1 i e3) (substitute e2 i e3))
substitute (Var x) i e1 | x == i = e1 | otherwise = (Var x)