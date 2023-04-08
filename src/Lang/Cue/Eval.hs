module Lang.Cue.Eval (eval) where

import "this" Prelude     hiding (negate, product, sum)

import Lang.Cue.Grammar
import Lang.Cue.Operation
import Lang.Cue.Value


--------------------------------------------------------------------------------
-- Evaluate an expression

eval :: Expression -> BaseValue
eval = resolve . evalExpression

evalExpression :: Expression -> Value
evalExpression = \case
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
  where
    run l r v f = foldl1 f $ map evalExpression $ l:r:v

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
    StringLiteral  t -> case t of
      Left  _ -> undefined
      Right s -> Atom $ StringAtom s
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
