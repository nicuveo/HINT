{-# LANGUAGE RankNTypes #-}

module Lang.Cue.Eval
 ( unify
 , disjoin
 ) where

import           Data.List.Extra (nubOrd)
import           Data.Sequence   (Seq (..), (<|), (|>))

import           Lang.Cue.Error
import           Lang.Cue.Value


--------------------------------------------------------------------------------
-- Internal dispatch for recursive defaults

class Eval a where
  eDisjoin :: a -> a -> a
  raise    :: a -> Value

instance Eval Value where
  eDisjoin = disjoin
  raise    = id

instance Eval BaseValue where
  eDisjoin = genericDisjoin
  raise    = promote


--------------------------------------------------------------------------------
-- Unification

unify :: Value -> Value -> Value
unify (WithDefault v1 d1) (WithDefault v2 d2) = WithDefault (v1 `genericUnify` v2) (d1 `genericUnify` d2)
unify (WithDefault v1 d1) (demote -> v2)      = WithDefault (v1 `genericUnify` v2) (d1 `genericUnify` v2)
unify (demote -> v1)      (WithDefault v2 d2) = WithDefault (v1 `genericUnify` v2) (v1 `genericUnify` d2)
unify v1 v2 = v1 `genericUnify` v2

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
  -- handled by the calling function
  (WithDefault _ _, _) -> unreachable
  (_, WithDefault _ _) -> unreachable
  where
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
        -- we do not check if l < h: it's not illegal for such a bound to exist
        -- but any value that attempts to unify with fail
        OrderedBound l h n
    unifyRegexes r1 r2 =
      -- TODO: deduplicate?
      r1 <> r2


--------------------------------------------------------------------------------
-- Disjunction

disjoin :: Value -> Value -> Value
disjoin (WithDefault v1 d1) (WithDefault v2 d2) = WithDefault (v1 `genericDisjoin` v2) (d1 `genericDisjoin` d2)
disjoin (WithDefault v1 d1) (demote -> v2)      = WithDefault (v1 `genericDisjoin` v2) d1
disjoin (demote -> v1)      (WithDefault v2 d2) = WithDefault (v1 `genericDisjoin` v2) d2
disjoin v1 v2 = v1 `genericDisjoin` v2

genericDisjoin :: Eval (CoreValue v) => CoreValue v -> CoreValue v -> CoreValue v
genericDisjoin = curry \case
  -- Bottom is the neutral element
  (Bottom e, _)        -> Bottom e
  (_, Bottom e)        -> Bottom e
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
