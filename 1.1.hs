import Data.List


data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 1
-- Seems to work well.

freeVariables :: Expr -> [Int]
freeVariables (Lam x e1) = freeVariables e1 \\ [x]
freeVariables (Var x) = [x]
freeVariables (App e1 e2) = (freeVariables e1) ++ (freeVariables e2)

-- Exercise 2
-- not fully functional - needs work. 

rename :: Expr -> Int -> Int -> Expr
rename (Lam x e1) v1 v2 | (x == v1) && (elem x (freeVariables e1)) = Lam (v2) (rename e1 v1 v2) | otherwise = Lam (x) (rename e1 v1 v2)
rename (Var x) v1 v2 | x == v1 = Var v2 | otherwise = Var x
rename (App e1 e2) v1 v2 = App (rename e1 v1 v2) (rename e2 v1 v2)

{-|
-- Exercise 3
alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent (Lam x e1) = 


-- If we rename all variables to a constant value, then compare strings?
-- We need to fix problems with 2 first though...

alphaEquivalent' :: Expr -> Int -> Int -> Expr
alphaEquivalent' (Lam x e1) v1 v2 | x == v1 = Lam (v2) (rename e1 v1 v2) | otherwise = Lam (x) (rename e1 v1 v2)
alphaEquivalent' (Var x) v1 v2 | x == v1 = Var v2 | otherwise = Var x
alphaEquivalent' (App e1 e2) v1 v2 = App (rename e1 v1 v2) (rename e2 v1 v2)

|-}
