module TypeProof where

import Type
import Grammar
import TypeResolver

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import qualified Data.List as List

substituteSystem :: EqSystem -> Type -> Type
substituteSystem [] t = t
substituteSystem ((a :=: b):eqs) t = substituteSystem eqs $ substitute t a b


splitExpr :: LExpr -> [LExpr]
splitExpr v@(V _) = [v]
splitExpr (_ :.: b) = [b]
splitExpr (a :-: b) = [a, b]


processContext :: EqSystem -> (LExpr, Resolver) -> State STMap Resolver
processContext eqs (expr, resolver) = do
  let newResult = substituteSystem eqs (resultType resolver) in do
    newDepend <- mapM (processContext eqs) (zip (splitExpr expr) (dependencies resolver))
    context <- get
    let newResolver = resolver { resultType = newResult,
                                 dependencies = newDepend } in
      case expr of
        v@(V _)   -> do
          put (Map.insert v newResult context)
          return $ newResolver
        (v :.: _) -> do
          put (Map.delete (V v) context)
          return $ newResolver
        _         -> do
          return $ newResolver


getIndent :: Int -> String
getIndent n = concat $ replicate n "*   "

ruleIdFromExpr :: LExpr -> Int
ruleIdFromExpr expr = case expr of
                        V _     -> 1
                        _ :-: _ -> 2
                        _       -> 3


showResolverProofI :: Int -> EqSystem -> (LExpr, Resolver) -> StateT STMap IO ()
showResolverProofI depth eqs (expr, resolver) = do
  context <- get
  let emptyCtx = Map.null context
      result = resultType resolver 
      depend = zip (splitExpr expr) (dependencies resolver) in
    let rid = ruleIdFromExpr expr in do
        liftIO $ putStrLn $
               getIndent depth
            ++ List.intercalate ", "
                 (Map.foldrWithKey
                   (\k a b -> (show k ++ " : " ++ show a) : b) [] context)
            ++ (if emptyCtx then "|- " else " |- ")
            ++ show expr ++ " : " ++ show result
            ++ " [rule #" ++ show rid ++ "]"
        case expr of
          a :.: _ -> do
            case result of
              t1 :>: _ -> do
                put (Map.insert (V a) t1 context)
                mapM_ (showResolverProofI (depth + 1) eqs) depend
                ctx <- get
                put (Map.delete (V a) ctx)
              _        -> undefined
          _       -> do
            mapM_ (showResolverProofI (depth + 1) eqs) depend

