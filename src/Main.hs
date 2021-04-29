module Main where

import LExprParser (getExprFromInput)
import TypeResolver
import Unification
import TypeProof

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  expr <- getExprFromInput
  let !resolver = buildResolver expr
  let !unified = unification $ system resolver
  case unified of
    Nothing  -> putStrLn "Expression has no type"
    Just uni -> let (new_res, context) = runState (processContext uni (expr, resolver)) Map.empty in do
                  _ <- runStateT (showResolverProofI 0 uni (expr, new_res)) context
                  return ()
