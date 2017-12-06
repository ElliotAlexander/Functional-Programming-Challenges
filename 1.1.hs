import Data.List


data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 1

freesub :: Expr -> [Int]
-- Continue computation minus input lambda-defined variables. 
freesub (Lam x e1) = freesub e1 \\ [x]
-- Has to be a pre-set var. 
freesub (Var x) = [x]
-- recursive combine each sub-expressions output. 
freesub (App e1 e2) = (freesub e1) ++ (freesub e2)