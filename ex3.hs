import Parsing

data ExtExpr = ExtApp ExtExpr ExtExpr| ExtLam [Int] ExtExpr | ExtVar Intderiving (Show, Eq)