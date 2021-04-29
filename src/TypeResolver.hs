module TypeResolver where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

import Type
import Grammar

data Equation = Type :=: Type deriving (Eq, Show)

type EqSystem = [Equation]

type STMap = Map.Map LExpr Type

data Resolver = Resolver { dependencies :: [Resolver],
                           resultType :: Type,
                           system :: EqSystem }
              deriving (Eq, Show)


data Scope = Scope { scope :: STMap, maxId :: Int }
           deriving (Eq, Show)


swapEquation :: Equation -> Equation
swapEquation (a :=: b) = b :=: a


getNewType :: LExpr -> State Scope Type
getNewType expr = do
  sc <- gets scope
  mId <- gets maxId
  put $ Scope sc (mId + 1)
  return $ T mId


getScopedType :: LExpr -> State Scope Type
getScopedType e@(V _) = do
  sc <- gets scope
  mId <- gets maxId
  if Map.member e sc
    then do
      return $ sc Map.! e
    else do
      newType <- getNewType e
      put $ Scope (Map.insert e newType sc) (mId + 1)
      return newType
getScopedType e = getNewType e


removeScopedType :: LExpr -> State Scope ()
removeScopedType e@(V _) = do
  sc <- gets scope
  mId <- gets maxId
  put $ Scope (Map.delete e sc) mId
removeScopedType _ = return ()


buildResolver :: LExpr -> Resolver
buildResolver expr = evalState (buildResolverI expr) $ Scope Map.empty 0


buildResolverI :: LExpr -> State Scope Resolver
buildResolverI e@(V _) = do
  resType <- getScopedType e
  return $ Resolver [] resType []


buildResolverI e@(e1 :-: e2) = do
  left <- buildResolverI e1
  right <- buildResolverI e2
  resType <- getScopedType e
  return $ Resolver [left, right] resType
             ([(resultType left) :=: ((resultType right) :>: resType)] 
               ++ (system left) ++ (system right))


buildResolverI (var :.: expr) = do
  varType <- getScopedType $ V var
  right <- buildResolverI expr
  removeScopedType $ V var
  return $ Resolver [right] (varType :>: (resultType right)) (system right)

