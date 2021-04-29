module Grammar where

newtype Var = Var String
            deriving (Eq, Ord)

data LExpr = V Var
           | Var :.: LExpr
           | LExpr :-: LExpr
           deriving (Eq, Ord)

instance Show Var where
    show (Var v) = v

instance Show LExpr where
    show (V var)   = show var
    show (v :.: e) = "(\\" ++ show v ++ ". " ++ show e ++ ")"
    show (e :-: f) = "(" ++ show e ++ " " ++ show f ++ ")"

