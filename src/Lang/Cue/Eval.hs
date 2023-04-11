module Lang.Cue.Eval (eval) where

import "this" Prelude     hiding (negate, product, sum)

import Data.Text qualified as T

import Lang.Cue.AST
import Lang.Cue.Operation
import Lang.Cue.Tokens
import Lang.Cue.Value


--------------------------------------------------------------------------------
-- Evaluate an expression

eval :: Expression -> BaseValue
eval = resolve . evalExpression

evalExpression :: Expression -> Value
evalExpression = \case
  Unary          u -> evalUnary u
  Multiplication l r -> go l r evalMultiplication
  Division       l r -> go l r evalDivision
  Addition       l r -> go l r evalAddition
  Subtraction    l r -> go l r evalSubtraction
  Equal          l r -> go l r evalEqual
  NotEqual       l r -> go l r evalNotEqual
  Match          l r -> go l r undefined -- evalMatch
  NotMatch       l r -> go l r undefined -- evalNotMatch
  LessThan       l r -> go l r evalLessThan
  LessOrEqual    l r -> go l r evalLessOrEqual
  GreaterThan    l r -> go l r evalGreaterThan
  GreaterOrEqual l r -> go l r evalGreaterOrEqual
  LogicalAnd     l r -> go l r undefined -- evalLogicalAnd
  LogicalOr      l r -> go l r undefined -- evalLogicalOr
  Unification    l r -> go l r evalUnification
  Disjunction    l r -> go l r evalDisjunction
  where
    go l r f = evalExpression l `f` evalExpression r

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

evalOperand :: Operand -> Value
evalOperand = \case
  OperandName _       -> undefined
  OperandExpression e -> evalExpression e
  OperandLiteral l    -> case l of
    IntegerLiteral i -> Atom $ IntegerAtom i
    FloatLiteral   f -> Atom $ FloatAtom f
    StringLiteral  t -> Atom $ StringAtom $ T.concat $ t <&> \elt ->
      case elt of
        RawStringLiteral _ x -> x
        Interpolation e -> case eval e of
          _ -> undefined
    BoolLiteral    b -> Atom $ BooleanAtom b
    NullLiteral      -> Null
    BottomLiteral    -> Bottom ArisedFromLiteral
    _                -> undefined

evalUnaryOperator :: Operator -> Value -> Value
evalUnaryOperator = undefined

resolve :: Value -> BaseValue
resolve = \case
  WithDefault v d -> case d of
    Bottom _ -> v
    _        -> d
  v -> demote v
