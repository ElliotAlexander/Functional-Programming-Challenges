data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)
data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- H-Expr -> Halfway expression
data HExpr = Happ HExpr HExpr | HK | HS | HVar Int | HLam Int HExpr

translate :: Expr -> ExprCL
translate e = out(ft(toH e))

toH :: Expr -> HExpr
toH (App e1 e2) = (Happ (toH e1) (toH e2))
toH (Lam i e1) = (HLam i (toH e1))
toH (Var x) = (HVar x)

out :: HExpr -> ExprCL
out (HVar x) = VarCL x
out (Happ e1 e2) = AppCL (out e1) (out e2)
out (HK) = K
out (HS) = S


ft :: HExpr -> HExpr
-- Rule 1
-- T[x] => x
ft (HVar x) = HVar x

-- Rule 2
-- T[(E₁E₂)] => (T[E₁]T[E₂])
ft (Happ e1 e2) = Happ (ft e1) (ft e2)

-- Rule 3 
-- T[λx->E] => (KT[E]) 
ft (HLam x e1) = Happ (HK) (ft e1)

-- Rule 4
-- T[\x->E] => (K T[E])
-- Note left associativity 
ft (HLam x (HVar a)) | x == a = (Happ (Happ HS HK) HK)

-- Rule 5
-- T[λx->λy->E] =>T[λx->T[λy->E]]  if X is free in E. 
ft (HLam x (HLam y e1)) | (hCheckFree e1 x) == True = (ft (HLam x (ft (HLam y (e1)))))

-- Rule 6
-- T[λx->(E₁E₂)] => (ST[λx->E₁]T[λx->E₂]) 
ft (HLam x (Happ e1 e2)) | (hCheckFree e1 x || hCheckFree e2 x) = Happ (Happ HS (ft (HLam x e1))) (ft (HLam x e2))




-- Our functions can't store free variables for the entire expression, so take an int
-- And check on a 1-1 basis.
hCheckFree :: HExpr -> Int -> Bool
hCheckFree (HVar x) i |  ((i == x) == True) = True
hCheckFree (HLam x e1) i | (hCheckFree e1 i == False) && (x /= i) = True | otherwise = False
hCheckFree (Happ e1 e2) i | (hCheckFree e1 i || hCheckFree e2 i) = True | otherwise = False
hCheckFree HK i = False
hCheckFree HS i = False
hCheckFree _ _ = False

