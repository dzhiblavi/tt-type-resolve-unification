module Type where

data Type = T Int
          | Type :>: Type
          deriving (Eq, Ord)

instance Show Type where
  show (T i) = "t" ++ show i
  show (t1 :>: t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

substitute :: Type -- input type
           -> Type -- T to be replaced
           -> Type -- by this type
           -> Type -- resulting type
substitute t@(T _) ti to = if t == ti then to else t
substitute (t1 :>: t2) ti to = (substitute t1 ti to) 
                           :>: (substitute t2 ti to)

