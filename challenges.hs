import Control.Applicative
import Data.Char


data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 1.1
-- Build a list of all variables, then remove them as we see them being bound
-- Lazy evaluation means we can build the list before starting to remove bound vars. 
freeVariables :: Expr -> [Int]
freeVariables (Lam x e1) = removeVal (freeVariables e1) x
freeVariables (Var x) = [x]
freeVariables (App e1 e2) = (freeVariables e1) ++ (freeVariables e2)

-- Remove a given variable from a list. I may use this later.
removeVal :: Eq a => [a] -> a -> [a]
removeVal [] _ = []
removeVal (x:xs) val
    | x == val = [] ++ removeVal xs val
    | otherwise = [x] ++ removeVal xs val


-- Exercise 1.2
rename :: Expr -> Int -> Int -> Expr 
-- For the first Lam where x = v1 we see bound, parse that inside rename'
rename (Lam x e1) v1 v2 | x == v1 = (Lam v2 (rename' e1 v1 v2))
rename (Lam x e1) v1 v2 = (Lam x (rename e1 v1 v2))
rename (Var x) v1 v2 | x == v1 = (Var v2) | otherwise = (Var x)
rename (App e1 e2) v1 v2 = App (rename e1 v1 v2) (rename e2 v1 v2)

-- Parse like normal, but once we see another Lam where x == v1 in the same expression, break (i.e. return to rename e1 0 0, presume no vars are named 0).
-- This will rename free variables of the same name as a bound variable, but idk whether this is acceptable .
-- i.e. App(App (Var 1) (Lam 1 (Var 1))) (Var 1)) => App (App (Var 2) (Lam 2 (Var 2))) (Var 2)
-- Where the first and last Var 1 are free. 
rename' :: Expr -> Int -> Int -> Expr
rename' (Lam x e1) v1 v2 | x == v1 = (Lam x (rename e1 0 0))
rename' (Lam x e1) v1 v2 = (Lam x (rename' e1 v1 v2))
rename' (Var x) v1 v2 | x == v1 = (Var v2)
rename' (Var x) v1 v2 = (Var x)
rename' (App e1 e2) v1 v2 = App (rename' e1 v1 v2) (rename' e2 v1 v2)

-- Exercise 1.3

-- Check for complete equivalence (e1 === e2), if fail then check for type equivalence. If type equivalence fails, we can safely return false.
-- Else, build a list of all possible variable conversions, then iterate using rename to build a list of complete equivalent expressions 
-- with variables renamed. 

-- If this list  > 0 , there MUST be at least one combination of renamed variables that works. 

alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent e1 e2 | completeequivalent e1 e2 = True | typeequivalent e1 e2 = renamer e1 e2 (canberenamed e1 e2 ) | otherwise = False

-- check for type equivalence
typeequivalent :: Expr -> Expr -> Bool
typeequivalent (Var _ ) (Var _ ) = True
typeequivalent (Lam _ e1) (Lam _ e2) = typeequivalent e1 e2
typeequivalent (App e1 e2) (App e3 e4) = ((typeequivalent e1 e3 && typeequivalent e2 e4) || (typeequivalent e1 e4 && typeequivalent e2 e3))
typeequivalent _ _ = False

-- Check for complete equivalence
completeequivalent :: Expr -> Expr -> Bool
completeequivalent (Var x) (Var y) = x == y
completeequivalent (Lam x e1) (Lam y e2) = ((x == y) && completeequivalent e1 e2)
completeequivalent (App e1 e2) (App e3 e4) = ((completeequivalent e1 e3 && completeequivalent e2 e4 ) || (completeequivalent e1 e4 && completeequivalent e2 e3))
completeequivalent _ _ = False

-- Build a list of variables which could be renamed. 
canberenamed :: Expr -> Expr -> [(Int, Int)]
canberenamed (App e1 e2) (App e3 e4)  = canberenamed e1 e3 ++ canberenamed e2 e4
canberenamed (Var x) (Var y) | x == y = [] | otherwise = [(x,y)]
canberenamed (Lam x e1) (Lam y e2) | x == y = canberenamed e1 e2 | otherwise = canberenamed e1 e2 ++ [(x,y)] 


-- This didn't quite work as intended, but was v.close.
--renamer :: Expr -> Expr -> [(Int, Int)] -> Bool
--renamer e1 e2 [] = False
--renamer e1 e2 (x:xs) = ((completeequivalent (rename e1 (fst x) (snd x)) e2) || renamer e1 e2 xs)

-- ITerate through all possible rename combos, check for equivalence. 
renamer :: Expr -> Expr -> [(Int, Int)] -> Bool
renamer e1 e2 xs = not (checkEmpty ([ a | a <- xs, completeequivalent (rename e1 (fst a) (snd a)) e2]))

-- Checks if the list is empty.
checkEmpty :: [a] -> Bool
checkEmpty [] = True
checkEmpty xs = False

-- Exercise 1.4

hasRedex :: Expr -> Bool
hasRedex (Lam x e1) = hasRedex e1
hasRedex (Var x) = False
hasRedex (App (Lam x e1) e2) = True
hasRedex (App e1 (Lam x e2)) = True
hasRedex (App e1 e2) = hasRedex e1 || hasRedex e2

--hasRedex' :: Expr -> Bool
--hasRedex' (Lam x e1) = True
--hasRedex' (App e1 e2) | (hasRedex' e1 == False && hasRedex' e2 == False) = False | otherwise = True
--hasRedex' (Var x) = False


-- Exercise 1.5

substitute :: Expr -> Int -> Expr -> Expr 
substitute (Lam x ex) var newe
    | x == var = (Lam x newe)
    -- if the new expression contains x, replace x with a new int inside the new expression
    -- Then substitute that renamed expression into the final expressiom.
    | contains (allvars newe) x = Lam next (substitute (rename ex x next) var newe)
    | otherwise = (Lam x (substitute ex var newe))
       where
        -- Get a new int not included in either expression.
            next = nextint ex newe
substitute (App e1 e2) var newe = (App (substitute e1 var newe) (substitute e1 var newe))
substitute (Var x) var newe
    | x == var = newe
    | otherwise = (Var x)


    -- Build a list of all variables.
allvars :: Expr -> [Int]
allvars (App e1 e2) = allvars e1 ++ allvars e2
allvars (Lam x e1) = [x] ++ allvars e1
allvars (Var x) = [x]

-- This just calls its recursive aux function, which iterates an int
nextint :: Expr -> Expr -> Int
nextint e1 e2  = nextint' e1 e2 1

-- Iterate an int until we find a valid i
nextint' :: Expr -> Expr -> Int -> Int
nextint' e1 e2 i
    | (contains (allvars e1 ++ allvars e2) i) == False = i
    | otherwise = nextint' e1 e2 (i + 1)

-- check if an int array contains x int.
contains ::  [Int] -> Int -> Bool
contains [] a = False
contains (x:xs) a
    | x == a = True
    | length xs == 0 = False
    | otherwise = contains xs a


-- Exercise 2
prettyPrint :: Expr -> String
-- For double Lams, keep stacking them to avoid \x1\x2\x3
prettyPrint (Lam x (Lam a e2)) = "\\x" ++ show x  ++ (prettyPrint' (Lam a e2)) 
prettyPrint (Lam x e1) = "\\x" ++ show x ++ "->" ++ (prettyPrint e1)

-- For Lambdas applied to functions, bracket the lambda.
prettyPrint (App (Lam x e1) e2) = "(" ++ prettyPrint (Lam x e1) ++ ")" ++ prettyPrint e2 
prettyPrint (App e2 (Lam x e1)) = prettyPrint e2 ++ "(" ++ prettyPrint (Lam x e2) ++ ")"

-- Bracket nested apps (left associative)
prettyPrint (App e3 (App e1 e2)) = (prettyPrint e3) ++ "(" ++ (prettyPrint (App e1 e2)) ++ ")"
prettyPrint (App e1 e2) = (prettyPrint e1) ++ (prettyPrint e2)
prettyPrint (Var x) = "x" ++ show x

-- Helper function to stack Lambdas. 
prettyPrint' :: Expr -> String
prettyPrint' (Lam x (Lam y e2)) = "x" ++ show x ++ (prettyPrint' (Lam y e2))
prettyPrint' (Lam x e1) = "x" ++ show x ++ "" ++ "->" ++ (prettyPrint' e1)
prettyPrint' e1 = prettyPrint e1




-- Exercise 4

-- Define a 'halway' data type. 
-- WE use HLam to take one extra step from HLAM -> HK/HS
data HExpr = Happ HExpr HExpr | HK | HS | HVar Int | HLam Int HExpr
data ExprCL = AppCL ExprCL ExprCL | K | S | VarCL Int deriving (Show)

-- H-Expr -> Halfway expression

translate :: Expr -> ExprCL
translate e = out(ft(toH e))

-- Convert ot a halfway type
toH :: Expr -> HExpr
toH (App e1 e2) = (Happ (toH e1) (toH e2))
toH (Lam i e1) = (HLam i (toH e1))
toH (Var x) = (HVar x)

-- Convert to an output type
out :: HExpr -> ExprCL
out (HVar x) = VarCL x
out (Happ e1 e2) = AppCL (out e1) (out e2)
out (HK) = K
out (HS) = S


-- Apply the combinatory logic rules.
ft :: HExpr -> HExpr
-- Rule 1
-- T[x] => x
ft (HVar x) = HVar x

-- Rule 2
-- T[(E₁E₂)] => (T[E₁]T[E₂])
ft (Happ e1 e2) = Happ (ft e1) (ft e2)

-- Rule 3 
-- T[λx->E] => (KT[E]) 
ft (HLam x e1) | (hCheckFree e1 x) == False = Happ (HK) (ft e1)

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
ft e = e



-- Our functions can't store free variables for the entire expression, so take an int
-- And check on a 1-1 basis.
hCheckFree :: HExpr -> Int -> Bool
hCheckFree (HVar x) i |  ((i == x) == True) = True
hCheckFree (HLam x e1) i | (hCheckFree e1 i) && (x /= i) = True | otherwise = False
hCheckFree (Happ e1 e2) i | (hCheckFree e1 i || hCheckFree e2 i) = True | otherwise = False
hCheckFree _ _ = False


