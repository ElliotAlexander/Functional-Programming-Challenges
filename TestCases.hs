-- Exercise 1
freeVariables (Lam 1(Lam 2 (App (Var 1) (App (Var 2) (Var 3))))) => [3]
freeVariables (App (Var 1) (Lam 1 (App (Var 2) (App (App (Lam 3 (App (Var 4) (Var 3))) (Var 2)) (Var 1))))) => [1,2,4,2]
freeVariables (Var 1) => 1
freeVariables (Lam 1 (App (Var 1) (Var 1))) => []

-- Exercise 2
rename (Lam 1 (App (Lam 1 (Var 1)) (Var 1))) 1 3 => Lam 3 (App (Lam 1 (Var 1)) (Var 3))
rename (Lam 1 (App (Lam 2 (App (Var 3) (Var 2))) (Var 2))) 2 5 => Lam 1 (App (Lam 5 (App (Var 3) (Var 5))) (Var 5))