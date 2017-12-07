-- Exercise 5
substitute :: Expr -> Int -> Expr -> Expr
substitute (Lam x e1) i e2 | x == i = (Lam e2 substitute(e1)) | otherwise = (Lam x (substitute(e1)))
substitute (App e1 e2) i e3 = (App (substitute e1) (substitute e2))
substitute (Var x) i e1 | x == i = e1 | otherwise = (Var x)