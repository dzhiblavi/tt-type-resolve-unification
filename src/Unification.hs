module Unification where

import Control.Monad.State.Strict
import Data.Maybe(fromJust, isNothing)

import TypeResolver
import Type


isIdentityEquation :: Equation -> Bool
isIdentityEquation (a :=: b) = a == b


discard :: EqSystem -> EqSystem
discard = filter (not . isIdentityEquation)


swapEqs :: EqSystem -> EqSystem
swapEqs = map condSwap
  where
    condSwap :: Equation -> Equation
    condSwap (a@(_ :>: _) :=: b@(T _)) = b :=: a
    condSwap eq = eq


unification :: EqSystem -> Maybe EqSystem
unification eqs = fst $ execState unificateProcess (Just eqs, False)


unificateProcess :: State (Maybe EqSystem, Bool) ()
unificateProcess = do
  (meqs, fin) <- get
  if fin || isNothing meqs
    then do
      return ()
    else do
      put (meqs, True)
      unificateStep $ fromJust meqs
      unificateProcess


unificateStep :: EqSystem -> State (Maybe EqSystem, Bool) ()
unificateStep eqs = let !filtered = swapEqs $ discard eqs in do
    put (Just filtered, True)
    unificateStepI 0 filtered


unificateStepI :: Int -> EqSystem -> State (Maybe EqSystem, Bool) ()
unificateStepI _ [] = do return ()
unificateStepI !idx ((a :=: b):eqs) = do
  case a of 
    (a1 :>: a2) -> case b of
                     (b1 :>: b2) -> do
                       allEqs <- gets fst
                       let headEqs = take idx (fromJust allEqs) in do
                         put (Just $ headEqs ++ [
                                a1 :=: b1,
                                a2 :=: b2
                              ] ++ eqs, False)
                     _           -> do
                         unificateStepI (idx + 1) eqs
    (T _)     -> do
      if not (checkSubstCorrectness a b)
        then do
          put (Nothing, False)
        else do
          allEqs <- gets fst
          let (newEqs, changed) 
                = trySubstitute idx (fromJust allEqs) a b in do
            if changed
              then do
                put (Just newEqs, False)
              else do
                unificateStepI (idx + 1) eqs


checkSubstCorrectness :: Type -> Type -> Bool
checkSubstCorrectness a b = case b of
  (T _)     -> a /= b
  (b1 :>: b2) -> (checkSubstCorrectness a b1) && (checkSubstCorrectness a b2)


trySubstitute :: Int -> EqSystem -> Type -> Type -> (EqSystem, Bool)
trySubstitute _ [] _ _ = ([], False)
trySubstitute !idx (eq@(a' :=: b'):eqs) a b =
  if idx == 0
    then 
      let (es, ch) = trySubstitute (idx - 1) eqs a b in
        (eq:es, ch)
    else
      let left  = substitute a' a b
          right = substitute b' a b in
            if (left /= a') || (right /= b')
              then
                let (es, _) = trySubstitute (idx - 1) eqs a b in
                  ((left :=: right):es, True)
              else
                let (es, ch) = trySubstitute (idx - 1) eqs a b in
                  (eq:es, ch)

