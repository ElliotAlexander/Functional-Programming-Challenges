import Data.List


data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 1

freeVariables :: Expr -> [Int]
-- Continue computation minus input lambda-defined variables. 
freeVariables (Lam x e1) = freeVariables e1 \\ [x]
-- Has to be a pre-set var. 
freeVariables (Var x) = [x]
-- recursive combine each sub-expressions output. 
freeVariables (App e1 e2) = (freeVariables e1) ++ (freeVariables e2)

-- Exercise 2
rename :: Expr -> Int -> Int -> Expr
rename (Lam x e1) v1 v2 | x == v1 = Lam (v2) (rename e1 v1 v2) | otherwise = Lam (x) (rename e1 v1 v2)
rename (Var x) v1 v2 | x == v1 = Var v2 | otherwise = Var x
rename (App e1 e2) v1 v2 = App (rename e1 v1 v2) (rename e2 v1 v2)

-- Exercise 3
alphaEquivalent :: Expr -> Expr -> Bool