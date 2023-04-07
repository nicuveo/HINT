{-# LANGUAGE RankNTypes #-}

module Lang.Cue.Operation
  ( evalUnification
  , evalDisjunction
  , evalUnaryPlus
  , evalUnaryMinus
  , evalAddition
  , evalSubtraction
  , evalMultiplication
  , evalDivision
  , evalEqual
  , evalNotEqual
  , evalLessThan
  , evalLessOrEqual
  , evalGreaterThan
  , evalGreaterOrEqual
  , evalNot
  ) where

import Data.List.Extra (nubOrd)
import Data.Sequence   (Seq (..), (<|), (|>))

import Lang.Cue.Error
import Lang.Cue.Value


--------------------------------------------------------------------------------
-- Internal dispatch for recursive defaults

class Eval a where
  eDisjoin :: a -> a -> a
  raise    :: a -> Value

instance Eval Value where
  eDisjoin = disjoin
  raise    = id

instance Eval BaseValue where
  eDisjoin = disjoin
  raise    = promote


--------------------------------------------------------------------------------
-- Unification

evalUnification :: Value -> Value -> Value
evalUnification = distribute2 genericUnify
  where
    genericUnify :: Eval (CoreValue v) => CoreValue v -> CoreValue v -> CoreValue v
    genericUnify = curry \case
      -- distribute over disjunction
      (Union u, v) -> foldl1 eDisjoin $ (`genericUnify` v) <$> u
      (v, Union u) -> foldl1 eDisjoin $ (v `genericUnify`) <$> u
      -- Top is the neutral element
      (Top, v) -> v
      (v, Top) -> v
      -- Bottom always wins
      (Bottom e, _) -> Bottom e
      (_, Bottom e) -> Bottom e
      -- Null unifies with nothing but itself
      (Null, Null) -> Null
      (Null, v   ) -> Bottom $ UnifyWithNull (raise v)
      (v,    Null) -> Bottom $ UnifyWithNull (raise v)
      -- bounds unify
      (Bound bx, Bound by) -> case (bx, by) of
        -- matching types
        (IntegerBound b1,  IntegerBound b2) -> Bound $ IntegerBound $ unifyOrderedBounds b1 b2
        (FloatBound   b1,  FloatBound   b2) -> Bound $ FloatBound   $ unifyOrderedBounds b1 b2
        (StringBound  b1,  StringBound  b2) -> Bound $ StringBound  $ unifyOrderedBounds b1 b2
        (BytesBound   b1,  BytesBound   b2) -> Bound $ BytesBound   $ unifyOrderedBounds b1 b2
        -- integer promotion
        (IntegerBound b1,  FloatBound   b2) -> Bound $ FloatBound   $ unifyOrderedBounds (fromInteger <$> b1) b2
        (FloatBound   b1,  IntegerBound b2) -> Bound $ FloatBound   $ unifyOrderedBounds b1 (fromInteger <$> b2)
        -- regexes
        (RegexBound   r1,  RegexBound   r2) -> Bound $ RegexBound   $ unifyRegexes r1 r2
        -- non-matching types
        (b1, b2) -> Bottom $ UnifyBounds b1 b2 -- IS THAT CORRECT?
      -- atoms do not unify with anything but themselves
      (Atom a1, Atom a2)
        | a1 == a2  -> Atom a1
        | otherwise -> Bottom $ UnifyAtoms a1 a2
      -- atoms and bounds unify if the value is within the bounds
      (Atom a, Bound b) -> checkBound a b
      (Bound b, Atom a) -> checkBound a b
      -- atoms and types unify if the value is of that type
      (Atom a, Type t) -> checkType a t
      (Type t, Atom a) -> checkType a t
      -- types do not unify with anything but themselves
      (Type t1, Type t2)
        | t1 == t2  -> Type t1
        | otherwise -> Bottom $ UnifyTypes t1 t2
      (Type t, v) -> Bottom $ UnifyTypeMismatch (Type t) (raise v)
      (v, Type t) -> Bottom $ UnifyTypeMismatch (raise v) (Type t)
      -- functions do not unify with anything but themselves
      (Function f1, Function f2)
        | f1 == f2  -> Function f1
        | otherwise -> Bottom $ UnifyFunctions f1 f2
      (Function f, v) -> Bottom $ UnifyTypeMismatch (Function f) (raise v)
      (v, Function f) -> Bottom $ UnifyTypeMismatch (raise v) (Function f)
      -- handled by the calling function
      (WithDefault _ _, _) -> unreachable
      (_, WithDefault _ _) -> unreachable
    checkType = curry \case
      (a@(BooleanAtom _), BooleanType) -> Atom a
      (a@(IntegerAtom _), IntegerType) -> Atom a
      (  (IntegerAtom i), FloatType)   -> Atom $ FloatAtom $ fromInteger i
      (a@(FloatAtom   _), FloatType)   -> Atom a
      (a@(StringAtom  _), StringType)  -> Atom a
      (a@(BytesAtom   _), BytesType)   -> Atom a
      (a, t) -> Bottom $ UnifyTypeMismatch (Atom a) (Type t)
    checkBound = curry \case
      (a@(IntegerAtom i), b@(IntegerBound c)) ->
        if checkOrdered c i then Atom a else Bottom $ UnifyOOB a b
      (a@(IntegerAtom i), b@(FloatBound   c)) ->
        if checkOrdered c (fromInteger i)     then Atom a else Bottom $ UnifyOOB a b
      (a@(FloatAtom   i), b@(IntegerBound c)) ->
        if checkOrdered (fromInteger <$> c) i then Atom a else Bottom $ UnifyOOB a b
      (a@(FloatAtom   i), b@(FloatBound   c)) ->
        if checkOrdered c i then Atom a else Bottom $ UnifyOOB a b
      (a@(StringAtom  s), b@(StringBound  c)) ->
        if checkOrdered c s then Atom a else Bottom $ UnifyOOB a b
      (a@(BytesAtom   s), b@(BytesBound   c)) ->
        if checkOrdered c s then Atom a else Bottom $ UnifyOOB a b
      (a, b) -> Bottom $ UnifyTypeMismatch (Atom a) (Bound b)
    unifyOrderedBounds (OrderedBound l1 h1 n1) (OrderedBound l2 h2 n2) =
      let
        l = case (l1, l2) of
          (Open, e) -> e
          (e, Open) -> e
          (Inclusive a, Inclusive b) -> if a > b then Inclusive a else Inclusive b
          (Inclusive a, Exclusive b) -> if a > b then Inclusive a else Exclusive b
          (Exclusive a, Inclusive b) -> if b > a then Inclusive b else Exclusive a
          (Exclusive a, Exclusive b) -> if b > a then Exclusive b else Exclusive a
        h = case (h1, h2) of
          (Open, e) -> e
          (e, Open) -> e
          (Inclusive a, Inclusive b) -> if a < b then Inclusive a else Inclusive b
          (Inclusive a, Exclusive b) -> if a < b then Inclusive a else Exclusive b
          (Exclusive a, Inclusive b) -> if b < a then Inclusive b else Exclusive a
          (Exclusive a, Exclusive b) -> if b < a then Exclusive b else Exclusive a
        n = nubOrd $ n1 <> n2
      in
        -- TODO: resolve to atom if possible
        OrderedBound l h n
    unifyRegexes r1 r2 =
      -- TODO: deduplicate?
      r1 <> r2


--------------------------------------------------------------------------------
-- Disjunction

evalDisjunction :: Value -> Value -> Value
evalDisjunction (WithDefault v1 d1) (WithDefault v2 d2) = WithDefault (v1 `disjoin` v2) (d1 `disjoin` d2)
evalDisjunction (WithDefault v1 d1) (demote -> v2)      = WithDefault (v1 `disjoin` v2) d1
evalDisjunction (demote -> v1)      (WithDefault v2 d2) = WithDefault (v1 `disjoin` v2) d2
evalDisjunction v1 v2 = v1 `disjoin` v2

disjoin :: Eval (CoreValue v) => CoreValue v -> CoreValue v -> CoreValue v
disjoin = curry \case
  -- Bottom is the neutral element
  (Bottom e, Bottom _) -> Bottom e
  (Bottom _, v)        -> v
  (v, Bottom _)        -> v
  -- Top always wins
  (Top, _)             -> Top
  (_, Top)             -> Top
  -- unify disjunctions
  -- TODO: how does this handle marking defaults?
  (Union u1, Union u2) -> Union $ u1 <> u2
  (Union u,  v)        -> Union $ u |> v
  (v, Union u)         -> Union $ v <| u
  -- otherwise, just create a new disjunction
  (u, v)               -> Union $ u :<| pure v


--------------------------------------------------------------------------------
-- Arithmetic operations

evalUnaryPlus :: Value -> Value
evalUnaryPlus = distribute1 \case
  a@(Atom (IntegerAtom _)) -> a
  a@(Atom (FloatAtom   _)) -> a
  v -> Bottom $ UnsupportedError $ raise v

evalUnaryMinus :: Value -> Value
evalUnaryMinus = distribute1 \case
  (Atom (IntegerAtom a)) -> Atom $ IntegerAtom (-a)
  (Atom (FloatAtom   a)) -> Atom $ FloatAtom   (-a)
  v -> Bottom $ UnsupportedError $ raise v

evalAddition :: Value -> Value -> Value
evalAddition = distribute2 $ curry \case
  (Atom (IntegerAtom a1), Atom (IntegerAtom a2)) -> Atom $ IntegerAtom $ a1 + a2
  (Atom (IntegerAtom a1), Atom (FloatAtom   a2)) -> Atom $ FloatAtom   $ fromInteger a1 + a2
  (Atom (FloatAtom   a1), Atom (IntegerAtom a2)) -> Atom $ FloatAtom   $ a1 + fromInteger a2
  (Atom (FloatAtom   a1), Atom (FloatAtom   a2)) -> Atom $ FloatAtom   $ a1 + a2
  (Atom (StringAtom  a1), Atom (StringAtom  a2)) -> Atom $ StringAtom  $ a1 <> a2
  (Atom (BytesAtom   a1), Atom (BytesAtom   a2)) -> Atom $ BytesAtom   $ a1 <> a2
  (a, b) -> Bottom $ UnifyTypeMismatch (raise a) (raise b)

evalSubtraction :: Value -> Value -> Value
evalSubtraction = arithmetic (-) (-)

evalMultiplication :: Value -> Value -> Value
evalMultiplication = arithmetic (*) (*)

evalDivision :: Value -> Value -> Value
evalDivision = arithmetic div (/)

arithmetic
  :: (Integer -> Integer -> Integer)
  -> (Double  -> Double  -> Double)
  -> (Value   -> Value   -> Value)
arithmetic i f = distribute2 $ curry \case
  (Atom (IntegerAtom a1), Atom (IntegerAtom a2)) -> Atom $ IntegerAtom $ a1 `i` a2
  (Atom (IntegerAtom a1), Atom (FloatAtom   a2)) -> Atom $ FloatAtom   $ fromInteger a1 `f` a2
  (Atom (FloatAtom   a1), Atom (IntegerAtom a2)) -> Atom $ FloatAtom   $ a1 `f` fromInteger a2
  (Atom (FloatAtom   a1), Atom (FloatAtom   a2)) -> Atom $ FloatAtom   $ a1 `f` a2
  (a, b) -> Bottom $ UnifyTypeMismatch (raise a) (raise b)


--------------------------------------------------------------------------------
-- Comparison

evalEqual :: Value -> Value -> Value
evalEqual = compareWith (==)

evalNotEqual :: Value -> Value -> Value
evalNotEqual = compareWith (/=)

evalLessThan :: Value -> Value -> Value
evalLessThan = orderWith (<)

evalLessOrEqual :: Value -> Value -> Value
evalLessOrEqual = orderWith (<=)

evalGreaterThan :: Value -> Value -> Value
evalGreaterThan = orderWith (>)

evalGreaterOrEqual :: Value -> Value -> Value
evalGreaterOrEqual = orderWith (>=)

compareWith :: (forall a. Eq a => a -> a -> Bool) -> Value -> Value -> Value
compareWith f = distribute2 $ curry \case
  -- Null is comparable
  (Null, Null) -> mkBool base
  (Null, _   ) -> mkBool $ not base
  (_,    Null) -> mkBool $ not base
  -- atoms compare type-wise
  (Atom (IntegerAtom a1), Atom (IntegerAtom a2)) -> mkBool $ a1 `f` a2
  (Atom (IntegerAtom a1), Atom (FloatAtom   a2)) -> mkBool $ fromInteger a1 `f` a2
  (Atom (FloatAtom   a1), Atom (IntegerAtom a2)) -> mkBool $ a1 `f` fromInteger a2
  (Atom (FloatAtom   a1), Atom (FloatAtom   a2)) -> mkBool $ a1 `f` a2
  (Atom (StringAtom  a1), Atom (StringAtom  a2)) -> mkBool $ a1 `f` a2
  (Atom (BytesAtom   a1), Atom (BytesAtom   a2)) -> mkBool $ a1 `f` a2
  (v1, v2) -> Bottom $ undefined v1 v2 -- FIXME
  where
    mkBool = Atom . BooleanAtom
    base = True `f` True

orderWith :: (forall a. Ord a => a -> a -> Bool) -> Value -> Value -> Value
orderWith f = distribute2 $ curry \case
  (Atom (IntegerAtom a1), Atom (IntegerAtom a2)) -> mkBool $ a1 `f` a2
  (Atom (IntegerAtom a1), Atom (FloatAtom   a2)) -> mkBool $ fromInteger a1 `f` a2
  (Atom (FloatAtom   a1), Atom (IntegerAtom a2)) -> mkBool $ a1 `f` fromInteger a2
  (Atom (FloatAtom   a1), Atom (FloatAtom   a2)) -> mkBool $ a1 `f` a2
  (Atom (StringAtom  a1), Atom (StringAtom  a2)) -> mkBool $ a1 `f` a2
  (Atom (BytesAtom   a1), Atom (BytesAtom   a2)) -> mkBool $ a1 `f` a2
  (v1, v2) -> Bottom $ undefined v1 v2 -- FIXME
  where
    mkBool = Atom . BooleanAtom


--------------------------------------------------------------------------------
-- Logical operators

evalNot :: Value -> Value
evalNot = distribute1 \case
  (Atom (BooleanAtom b)) -> Atom $ BooleanAtom $ not b
  _ -> Bottom undefined -- FIXME


--------------------------------------------------------------------------------
-- Helpers

distribute1 :: (forall v. Eval (CoreValue v) => CoreValue v -> CoreValue v) -> Value -> Value
distribute1 f (WithDefault v d) = WithDefault (f v) (f d)
distribute1 f v                 = f v

distribute2 :: (forall v. Eval (CoreValue v) => CoreValue v -> CoreValue v -> CoreValue v) -> Value -> Value -> Value
distribute2 f (WithDefault v1 d1) (WithDefault v2 d2) = WithDefault (v1 `f` v2) (d1 `f` d2)
distribute2 f (WithDefault v1 d1) (demote -> v2)      = WithDefault (v1 `f` v2) (d1 `f` v2)
distribute2 f (demote -> v1)      (WithDefault v2 d2) = WithDefault (v1 `f` v2) (v1 `f` d2)
distribute2 f v1 v2 = v1 `f` v2
