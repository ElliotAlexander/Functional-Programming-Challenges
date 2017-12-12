data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 1
-- Seems to work well.

freeVariables :: Expr -> [Int]
freeVariables (Lam x e1) = removeVal (freeVariables e1) x
freeVariables (Var x) = [x]
freeVariables (App e1 e2) = (freeVariables e1) ++ (freeVariables e2)

removeVal :: Eq a => [a] -> a -> [a]
removeVal (x:[]) val | x == val = [x] | otherwise = []
removeVal (x:xs) val
    | x == val = [] ++ removeVal xs val
    | otherwise = [x] ++ removeVal xs val


-- Exercise 2

rename :: Expr -> Int -> Int -> Expr 
-- Check that the next expression isn't a lam of the same value, if it is break.
rename (Lam x e1) v1 v2 | x == v1 = (Lam v2 (rename' e1 v1 v2))
rename (Lam x e1) v1 v2 = (Lam x (rename e1 v1 v2))
rename (Var x) v1 v2 | x == v1 = (Var v2) | otherwise = (Var x)
rename (App e1 e2) v1 v2 = App (rename e1 v1 v2) (rename e2 v1 v2)

rename' :: Expr -> Int -> Int -> Expr
rename' (Lam x e1) v1 v2 | x == v1 = (Lam x (rename' e1 0 0))
rename' (Lam x e1) v1 v2 = (Lam x (rename' e1 v1 v2))
rename' (Var x) v1 v2 | x == v1 = (Var v2)
rename' (Var x) v1 v2 = (Var  x)
rename' (App e1 e2) v1 v2 = App (rename' e1 v1 v2) (rename' e2 v1 v2)


-- Exercise 3

-- Still needs fixing

alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent (Var x) (Var y) | x == y = True | otherwise = False
alphaEquivalent e1 e2 | (areExprEqual e1 e2) == True = True | otherwise = False


areExprEqual :: Expr -> Expr -> Bool
areExprEqual (Var x) (Var y) | x == y = True | otherwise = False
areExprEqual (Lam x e1) (Lam y e2) | (x == y) && (areExprEqual e1 e2) = True | otherwise = False
areExprEqual (App e1 e2) (App e3 e4) | ((e1 == e3 && e2 == e4)||(e1 == e4 && e2 == e3)) = True | otherwise = False

boundVariables :: Expr -> [Int]
boundVariables (Lam x e1) = [x] ++ boundVariables e1
boundVariables (App e1 e2) = boundVariables e1 ++ boundVariables e2
boundVariables (Var x) = [x]
 
contains ::  [Int] -> Int -> Bool
contains [] a = False
contains (x:xs) a
    | x == a = True
    | length xs == 0 = False
    | otherwise = contains xs a



-- Exercise 4

hasRedex :: Expr -> Bool
hasRedex (Lam x e1) = hasRedex' e1
hasRedex (Var x) = False
hasRedex (App e1 e2) = hasRedex' (App e1 e2)

hasRedex' :: Expr -> Bool
hasRedex' (Lam x e1) = True
hasRedex' (App e1 e2) | (hasRedex' e1 == False && hasRedex' e2 == False) = False | otherwise = True
hasRedex' (Var x) = False


-- Exercise 5
    
substitute :: Expr -> Int -> Expr -> Expr 
substitute e1 i e2 = substitute' e1 i e2 (freeVariables e1)

substitute' :: Expr -> Int -> Expr -> [Int] -> Expr
substitute' (Lam x e1) i e2 freevars = (Lam x (substitute' e1 i e2 freevars))
substitute' (Var x) i e1 freevars
    | (x == i) && (contains freevars i) = e1 
    | otherwise = (Var x) 
substitute' (App e1 e2) i e3 freevars = (App (substitute' e1 i e3 freevars) (substitute' e2  i e3 freevars))

containsLam :: Expr -> Int -> Bool
containsLam (App e1 e2) i = (containsLam e1 i) && (containsLam e2 i) 
containsLam (Lam a e1) i | i == a = True | otherwise = containsLam e1 i
containsLam (Var x) i = False


-- Pretty Printer :)

prettyPrint :: Expr -> String
prettyPrint (Lam x (Lam a e2)) = "\\x" ++ show x  ++ (prettyPrint' (Lam a e2)) 
prettyPrint (Lam x e1) = "\\x" ++ show x ++ "->" ++ (prettyPrint e1)
prettyPrint (App (Lam x e1) e2) = "(" ++ prettyPrint (Lam x e1) ++ ")" ++ prettyPrint e2 
prettyPrint (App e2 (Lam x e1)) = prettyPrint e2 ++ "(" ++ prettyPrint (Lam x e2) ++ ")"
prettyPrint (App e3 (App e1 e2)) = (prettyPrint e3) ++ "(" ++ (prettyPrint (App e1 e2)) ++ ")"
prettyPrint (App e1 e2) = (prettyPrint e1) ++ (prettyPrint e2)
prettyPrint (Var x) = "x" ++ show x

prettyPrint' :: Expr -> String
prettyPrint' (Lam x (Lam y e2)) = "x" ++ show x ++ (prettyPrint' (Lam y e2))
prettyPrint' (Lam x e1) = "x" ++ show x ++ "" ++ "->" ++ (prettyPrint' e1)
prettyPrint' e1 = prettyPrint e1