data Expr = App Expr Expr | Lam Int Expr | Var Int deriving (Show, Eq)

-- Exercise 1
-- Seems to work well.
freeVariables :: Expr -> [Int]
freeVariables (Lam x e1) = removeVal (freeVariables e1) x
freeVariables (Var x) = [x]
freeVariables (App e1 e2) = (freeVariables e1) ++ (freeVariables e2)

removeVal :: Eq a => [a] -> a -> [a]
removeVal [] _ = []
removeVal (x:xs) val
    | x == val = [] ++ removeVal xs val
    | otherwise = [x] ++ removeVal xs val


-- Exercise 2
-- Needs fixing
rename :: Expr -> Int -> Int -> Expr 
-- Check that the next expression isn't a lam of the same value, if it is break.
rename (Lam x e1) v1 v2 | x == v1 = (Lam v2 (rename' e1 v1 v2))
rename (Lam x e1) v1 v2 = (Lam x (rename e1 v1 v2))
rename (Var x) v1 v2 | x == v1 = (Var v2) | otherwise = (Var x)
rename (App (Lam x e1) (Lam y e2)) v1 v2 = (App (rename' (Lam x e1) v1 v2) (rename' (Lam y e2) v1 v2))
rename (App (Lam x e1) e2) v1 v2 = (App (rename' (Lam x e1) v1 v2) (rename e2 v1 v2))
rename (App e1 (Lam x e2)) v1 v2 = (App (rename e1 v1 v2) (rename' (Lam x e2) v1 v2))
rename (App e1 e2) v1 v2 = App (rename e1 v1 v2) (rename e2 v1 v2)

rename' :: Expr -> Int -> Int -> Expr
rename' (Lam x e1) v1 v2 | x == v1 = (Lam x (rename' e1 0 0))
rename' (Lam x e1) v1 v2 = (Lam x (rename' e1 v1 v2))
rename' (Var x) v1 v2 | x == v1 = (Var v2)
rename' (Var x) v1 v2 = (Var x)
rename' (App e1 e2) v1 v2 = App (rename' e1 v1 v2) (rename' e2 v1 v2)


-- Exercise 3

-- Still needs fixing

alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent e1 e2
    | completeequivalent e1 e2 = True
    | typeequivalent e1 e2 = renamer e1 e2 (canberenamed e1 e2)


typeequivalent :: Expr -> Expr -> Bool
typeequivalent (Var _ ) (Var _ ) = True
typeequivalent (Lam _ e1) (Lam _ e2) = typeequivalent e1 e2
typeequivalent (App e1 e2) (App e3 e4) = ((typeequivalent e1 e3 && typeequivalent e2 e4) || (typeequivalent e1 e4 && typeequivalent e2 e3))
typeequivalent _ _ = False

completeequivalent :: Expr -> Expr -> Bool
completeequivalent (Var x) (Var y) = x == y
completeequivalent (Lam x e1) (Lam y e2) = ((x == y) && completeequivalent e1 e2)
completeequivalent (App e1 e2) (App e3 e4) = ((completeequivalent e1 e3 && completeequivalent e2 e4 ) || (completeequivalent e1 e4 && completeequivalent e2 e3))
completeequivalent _ _ = False

canberenamed :: Expr -> Expr -> [(Int, Int)]
canberenamed (App e1 e2) (App e3 e4) = canberenamed e1 e3 ++ canberenamed e2 e4
canberenamed (Var x) (Var y) 
    | x == y = [] 
    | otherwise = [(x,y)]
canberenamed (Lam x e1) (Lam y e2)
    | x == y = canberenamed e1 e2 
    | otherwise = canberenamed e1 e2 ++ [(x,y)] 

renamer :: Expr -> Expr -> [(Int, Int)] -> Bool
renamer e1 e2 [] = False
renamer e1 e2 (x:xs) = ((completeequivalent (rename e1 (fst x) (snd x)) e2) || renamer e1 e2 xs)

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
substitute (Lam x ex) var newe
    | x == var = (Lam x newe)
    | contains (allvars newe) x = Lam next (substitute (rename ex x next) var newe)
    | otherwise = (Lam x (substitute ex var newe))
       where
            next = nextint ex newe
substitute (App e1 e2) var newe = (App (substitute e1 var newe) (substitute e1 var newe))
substitute (Var x) var newe
    | x == var = newe
    | otherwise = (Var x)


allvars :: Expr -> [Int]
allvars (App e1 e2) = allvars e1 ++ allvars e2
allvars (Lam x e1) = [x] ++ allvars e1
allvars (Var x) = [x]

nextint :: Expr -> Expr -> Int
nextint e1 e2 = [new | new <- [1..], new /= (max' (allvars e1 ++ allvars e2))] !! 0

max' :: (Ord a) => [a] -> a
max' [x] = x
max' (x:xs)
    | (x > max' xs) = x
    | otherwise = max' xs

contains ::  [Int] -> Int -> Bool
contains [] a = False
contains (x:xs) a
    | x == a = True
    | length xs == 0 = False
    | otherwise = contains xs a


-- Pretty Printer :)
-- Needs fixing

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