module Lang.Cue.Eval
 ( eval
 ) where

import           Prelude         hiding (product, sum, negate)

import           Lang.Cue.Value
import           Lang.Cue.Grammar
import           Lang.Cue.Operation


--------------------------------------------------------------------------------
-- Evaluate an expression

eval :: Expression -> BaseValue
eval = resolve . eval'
  where
    eval' = \case
      Unary          u -> evalUnary u
      Multiplication l r v -> run l r v evalMultiplication
      Division       l r v -> run l r v evalDivision
      Addition       l r v -> run l r v evalAddition
      Subtraction    l r v -> run l r v evalSubtraction
      Equal          l r v -> run l r v evalEqual
      NotEqual       l r v -> run l r v evalNotEqual
      Match          l r v -> run l r v undefined -- evalMatch
      NotMatch       l r v -> run l r v undefined -- evalNotMatch
      LessThan       l r v -> run l r v evalLessThan
      LessOrEqual    l r v -> run l r v evalLessOrEqual
      GreaterThan    l r v -> run l r v evalGreaterThan
      GreaterOrEqual l r v -> run l r v evalGreaterOrEqual
      LogicalAnd     l r v -> run l r v undefined -- evalLogicalAnd
      LogicalOr      l r v -> run l r v undefined -- evalLogicalOr
      Unification    l r v -> run l r v evalUnification
      Disjunction    l r v -> run l r v evalDisjunction
    run l r v f = foldl1 f $ map eval' $ l:r:v

evalUnary :: UnaryExpression -> Value
evalUnary (UnaryExpression operators primaryExpression) =
  foldr evalUnaryOperator (evalPrimary primaryExpression) operators

evalPrimary :: PrimaryExpression -> Value
evalPrimary = \case
  PrimaryOperand  o   -> evalOperand o
  PrimaryCall     o _ -> evalPrimary o
  PrimarySelector _ _ -> undefined
  PrimaryIndex    _ _ -> undefined
  PrimarySlice    _ _ -> undefined

evalOperand :: a
evalOperand = undefined

evalUnaryOperator :: Operator -> Value -> Value
evalUnaryOperator = undefined

resolve :: Value -> BaseValue
resolve = undefined
