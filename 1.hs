import qualified Data.List as List
import qualified Data.Set as Set

data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 1
-- Seems to work well.

freeVariables :: Expr -> [Int]
freeVariables (Lam x e1) = freeVariables e1 List.\\ [x]
freeVariables (Var x) = [x]
freeVariables (App e1 e2) = (freeVariables e1) ++ (freeVariables e2)

-- Exercise 2
-- not fully functional - needs work. 

rename :: Expr -> Int -> Int -> Expr
rename (Lam x e1) v1 v2 | x == v1 && (elem x (freeVariables e1)) = Lam (v2) (rename e1 v1 v2) | otherwise = Lam (x) (rename e1 v1 v2)
rename (Var x) v1 v2 | x == v1 = Var v2 | otherwise = Var x
rename (App e1 e2) v1 v2 = App (rename e1 v1 v2) (rename e2 v1 v2)


-- Exercise 3

--alphaEquivalent :: Expr -> Expr -> Bool
--alphaEquivalent (Lam x e1) (Lam y e2) = alphaEquivalent e1 e2
--alphaEquivalent (Var x) (Var y) = True
--alphaEquivalent (App e1 e2) (App e3 e4) | e1 == e3 && e2 == e4 =  | 


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
substitute (Lam x e1) i e2 | x == i = (App e2 (substitute e1 i e2)) | otherwise = (Lam x (substitute e1 i e2))
substitute (App e1 e2) i e3 = (App (substitute e1 i e3) (substitute e2 i e3))
substitute (Var x) i e1 | x == i = e1 | otherwise = (Var x)



-- Pretty Printer :)
prettyPrint :: Expr -> String
prettyPrint (Lam x (Lam a e2)) = "\\x" ++ show x  ++ (prettyPrint' (Lam a e2)) 
prettyPrint (Lam x e1) = "\\x" ++ show x ++ "->" ++ (prettyPrint e1)
prettyPrint (App e1 e2) = (prettyPrint e1) ++ (prettyPrint e2)
prettyPrint (Var x) = "x" ++ show x

prettyPrint' :: Expr -> String
prettyPrint' (Lam x (Lam y e2)) = "x" ++ show x ++ (prettyPrint' (Lam y e2))
prettyPrint' (Lam x e1) = "x" ++ show x ++ "" ++ "->" ++ (prettyPrint' e1)
prettyPrint' e1 = prettyPrint e1