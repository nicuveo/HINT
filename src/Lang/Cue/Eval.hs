{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo     #-}

module Lang.Cue.Eval where

{-
  ( inlineAliases
  , eval
  ) where
-}

import "this" Prelude

import Control.Lens                    hiding (Empty, List, below, op)
import Control.Monad.Base
import Control.Monad.ST
import Data.HashMap.Strict             qualified as M
import Data.List                       (nub)
import Data.Sequence                   as S hiding (length)
import Data.STRef
import Data.Text                       qualified as T
import Data.Text.Encoding              (encodeUtf8)
import Regex.RE2                       qualified as RE2

import Lang.Cue.Document               as D
import Lang.Cue.Error
import Lang.Cue.Internal.HKD
import Lang.Cue.Internal.IndexedPlated
import Lang.Cue.IR                     as I
import Lang.Cue.Location


--------------------------------------------------------------------------------
-- * Evaluation monad

data ThunkCacheNode s = ThunkCacheNode
  { nodeThunk  :: Thunk
  , evalStatus :: Bool
  , subPaths   :: HashMap PathElem (STRef s (ThunkCacheNode s))
  }

type ThunkPtr s = STRef s (ThunkCacheNode s)

{-
data EvalStatus
  = -- | no evaluation has happened; attempting to access a field will fail with
    -- 'CannotBeEvaluatedYet'
    NotEvaluatedYet
  | -- | embeddings have been processed, and the result has been unified with
    -- the declarations: accessing a field should be allowed for the purpose of
    -- evaluating string fields i guess?
  | EmbeddingsResolved
  | AllFieldsUnified
-}

data EvalContext s = EvalContext
  { cacheRoot   :: STRef s (ThunkCacheNode s)
  , currentEval :: [Path]
  }

data EvalError
  = CannotBeEvaluatedYet
  | EvaluationFailed (Seq BottomSource)

newtype Eval s a = Eval
  { runEval :: ExceptT EvalError (StateT (EvalContext s) (ST s)) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (EvalContext s)
    , MonadError EvalError
    , MonadFix
    )

instance MonadBase (ST s) (Eval s) where
  liftBase = Eval . liftBase

eval :: Thunk -> Either Errors Document
eval t = translateError $ runST go
  where
    go :: forall s. ST s (Either EvalError Document)
    go =  do
      root <- newSTRef $ ThunkCacheNode @s t False M.empty
      let ctx = EvalContext root []
      flip evalStateT ctx $ runExceptT $ runEval $ fullyEval root
    translateError = \case
      Left (EvaluationFailed b) ->
        Left  $ fmap (withLocation (Location "" [] 0) . BottomError) b
      Left CannotBeEvaluatedYet ->
        Right $ Thunk t
      Right d ->
        Right d


--------------------------------------------------------------------------------
-- * Evaluation

fullyEval :: ThunkPtr s -> Eval s Document
fullyEval ptr = do
  ThunkCacheNode {..} <- liftBase $ readSTRef ptr
  resolve `orRecoverWith` nodeThunk

resolve :: Thunk -> Eval s Document
resolve = evalToNF >=> \case
  Disjoint v d -> case (v, d) of
    (      Empty,       Empty) -> unreachable
    (x :<| Empty,       Empty) -> pure x
    (_          ,       Empty) -> throwError CannotBeEvaluatedYet
    (_          , x :<| Empty) -> pure x
    (_          , _          ) -> throwError CannotBeEvaluatedYet
  d -> pure d

evalToNF :: Thunk -> Eval s Document
evalToNF t = case t of
  Disjunction        d  -> evalDisjunction d
  Unification        u  -> evalUnification u
  LogicalOr        l r  -> join $ evalOr   <$> resolve l <*> resolve r
  LogicalAnd       l r  -> join $ evalAnd  <$> resolve l <*> resolve r
  Equal            l r  -> join $ evalEq   <$> resolve l <*> resolve r
  NotEqual         l r  -> join $ evalNEq  <$> resolve l <*> resolve r
  Match            l r  -> join $ evalRE   <$> resolve l <*> resolve r
  NotMatch         l r  -> join $ evalNRE  <$> resolve l <*> resolve r
  LessThan         l r  -> join $ evalLT   <$> resolve l <*> resolve r
  LessOrEqual      l r  -> join $ evalLE   <$> resolve l <*> resolve r
  GreaterThan      l r  -> join $ evalGT   <$> resolve l <*> resolve r
  GreaterOrEqual   l r  -> join $ evalGE   <$> resolve l <*> resolve r
  Addition         l r  -> join $ evalAdd  <$> resolve l <*> resolve r
  Subtraction      l r  -> join $ evalSub  <$> resolve l <*> resolve r
  Multiplication   l r  -> join $ evalMul  <$> resolve l <*> resolve r
  Division         l r  -> join $ evalDiv  <$> resolve l <*> resolve r
  NumId              n  -> evalPlus =<< resolve n
  Negate             n  -> evalNeg  =<< resolve n
  LogicalNot         n  -> evalNot  =<< resolve n
  IsNotEqualTo       n  -> evalUNE  =<< resolve n
  Matches            n  -> evalURE  =<< resolve n
  Doesn'tMatch       n  -> evalUNRE =<< resolve n
  IsLessThan         n  -> evalULT  =<< resolve n
  IsLessOrEqualTo    n  -> evalULE  =<< resolve n
  IsGreaterThan      n  -> evalUGT  =<< resolve n
  IsGreaterOrEqualTo n  -> evalUGE  =<< resolve n
  Select _ _            -> undefined
  Index  _ _            -> undefined
  Slice  _ _ _          -> undefined
  Call _fun _args       -> undefined
  I.List  ListInfo {..} -> D.List <$> evalList listElements
  Block _b              -> undefined
  Interpolation _ _ts   -> undefined -- T.concat <$> traverse evalToString ts
  Ref _absolutePath     -> undefined
  Alias _p _l           -> error "unevaluated alias"
  Leaf a                -> pure $ Atom  a
  I.Type c              -> pure $ D.Type c
  Func  _               -> pure $ Thunk t
  Top                   -> pure $ Thunk t
  Bottom                -> report ArisedFromLiteral

evalDisjunction :: Seq (Bool, Thunk) -> Eval s Document
evalDisjunction disj = mdo
  (result, starred, errors, marked) <-
    disj & flip foldlM (S.empty, S.empty, S.empty, False) \(r, s, e, m) (star, thunk) ->
      catchBottom (evalToNF thunk) <&> \case
        Left  err -> (r, s, e <> err, m || star)
        Right doc ->
          let (val, def) = case doc of
                Disjoint v Empty -> (v, if star then v else Empty)
                Disjoint v d     -> (v, if star || not marked then d else Empty)
                _                -> (pure doc, if star then pure doc else Empty)
          in  (r <> val, s <> def, e, m || star)
  if S.null result
    then throwError $ EvaluationFailed errors
    else pure $ Disjoint (nubBy mergeable result) (nubBy mergeable starred)
  where
    mergeable :: Document -> Document -> Bool
    mergeable = (==) `on` (Mergeable . abstract Mergeable)

evalUnification :: Seq Thunk -> Eval s Document
evalUnification = go I.Top
  where
    go l Empty     = evalToNF l
    go l (r :<| u) = case (l, r) of
      (_, Unification x)     -> go l (x <> u)
      (_, Select        {})  -> undefined -- retrieveThunk r >>= \e -> go l (e :<| u)
      (_, Index         {})  -> undefined -- retrieveThunk r >>= \e -> go l (e :<| u)
      (_, Slice         {})  -> undefined -- retrieveThunk r >>= \e -> go l (e :<| u)
      (_, Ref           {})  -> undefined -- retrieveThunk r >>= \e -> go l (e :<| u)
      (_, Call          {})  -> undefined
      (_, Alias         {})  -> error "unevaluated alias"         -- FIXME
      (_, Interpolation {})  -> error "unevaluated interpolation" -- FIXME
      (Top,    _)            -> go r u
      (Bottom, _)            -> report ArisedFromLiteral
      (_, Top   )            -> go l u
      (_, Bottom)            -> report ArisedFromLiteral
      (Disjunction d, t)     -> go (Disjunction $ d <<&>> \e -> Unification [e, t]) u
      (t, Disjunction d)     -> go (Disjunction $ d <<&>> \e -> Unification [t, e]) u
      (I.List l1, I.List l2) -> unifyLists  l1 l2 >>= flip go u
      (Block  b1, Block  b2) -> unifyBlocks b1 b2 >>= flip go u
      _                      -> join $ unify <$> evalToNF l <*> go r u

unifyLists :: ListInfo -> ListInfo -> Eval s Thunk
unifyLists (ListInfo e1 c1) (ListInfo e2 c2) = do
  l1 <- concat <$> traverse retrieveEmbeddingThunks e1
  l2 <- concat <$> traverse retrieveEmbeddingThunks e2
  let n1 = length l1
      n2 = length l2
  when (n1 < n2 && isNothing c1) $ error "list length mismatch"
  when (n2 < n1 && isNothing c2) $ error "list length mismatch"
  let c3 = case (c1, c2) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just t2) -> Just t2
        (Just t1, Nothing) -> Just t1
        (Just t1, Just t2) -> Just $ Unification [t1, t2]
      l3 = paddedZipWith merge (fromMaybe Top c1) (fromMaybe Top c2) l1 l2
  pure $ I.List $ ListInfo (fromList l3) c3
  where
    merge t1 t2 = InlineThunk $ Unification [t1, t2]

unifyBlocks _ _ = undefined

unify d1 d2 = case (d1, d2) of
  (Atom   a1, Atom   a2) | a1 == a2        -> pure d1
  (Atom   a1, NotNull  ) | a1 /= Null      -> pure d1
  (NotNull,   Atom   a2) | a2 /= Null      -> pure d2
  (NotNull,   NotNull  )                   -> pure NotNull

  (D.Type IntegerType, D.Type IntegerType) -> pure d1
  (D.Type IntegerType, D.Type  NumberType) -> pure d1
  (D.Type  NumberType, D.Type IntegerType) -> pure d2
  (D.Type   FloatType, D.Type   FloatType) -> pure d1
  (D.Type   FloatType, D.Type  NumberType) -> pure d1
  (D.Type  NumberType, D.Type   FloatType) -> pure d2
  (D.Type  NumberType, D.Type  NumberType) -> pure d1
  (D.Type  StringType, D.Type  StringType) -> pure d1
  (D.Type   BytesType, D.Type   BytesType) -> pure d1
  (D.Type BooleanType, D.Type BooleanType) -> pure d1

  (D.Type IntegerType, IntegerBound _)     -> pure d2
  (D.Type  NumberType, IntegerBound _)     -> pure d2
  (D.Type   FloatType,   FloatBound _)     -> pure d2
  (D.Type  NumberType,   FloatBound _)     -> pure d2
  (D.Type  StringType,  StringBound _)     -> pure d2
  (D.Type   BytesType,   BytesBound _)     -> pure d2
  (IntegerBound _, D.Type IntegerType)     -> pure d1
  (IntegerBound _, D.Type  NumberType)     -> pure d1
  (  FloatBound _, D.Type   FloatType)     -> pure d1
  (  FloatBound _, D.Type  NumberType)     -> pure d1
  ( StringBound _, D.Type  StringType)     -> pure d1
  (  BytesBound _, D.Type   BytesType)     -> pure d1

  (Atom (Integer x), IntegerBound b)       -> d1 <$ checkI x b
  (Atom (Float   x), FloatBound   b)       -> d1 <$ checkF x b
  (Atom (String  x), StringBound  b)       -> d1 <$ checkS x b
  (Atom (Bytes   x), BytesBound   b)       -> d1 <$ checkB x b
  (IntegerBound b, Atom (Integer x))       -> d2 <$ checkI x b
  (FloatBound   b, Atom (Float   x))       -> d2 <$ checkF x b
  (StringBound  b, Atom (String  x))       -> d2 <$ checkS x b
  (BytesBound   b, Atom (Bytes   x))       -> d2 <$ checkB x b

  (Atom (Integer _), D.Type IntegerType)   -> pure d1
  (Atom (Integer _), D.Type  NumberType)   -> pure d1
  (Atom (Float   _), D.Type   FloatType)   -> pure d1
  (Atom (Float   _), D.Type  NumberType)   -> pure d1
  (Atom (String  _), D.Type  StringType)   -> pure d1
  (Atom (Bytes   _), D.Type   BytesType)   -> pure d1
  (D.Type IntegerType, Atom (Integer _))   -> pure d2
  (D.Type  NumberType, Atom (Integer _))   -> pure d2
  (D.Type   FloatType, Atom (Float   _))   -> pure d2
  (D.Type  NumberType, Atom (Float   _))   -> pure d2
  (D.Type  StringType, Atom (String  _))   -> pure d2
  (D.Type   BytesType, Atom (Bytes   _))   -> pure d2

  (IntegerBound i1, IntegerBound i2)       -> mergeBound Integer IntegerBound i1 i2
  (FloatBound   f1, FloatBound   f2)       -> mergeBound Float   FloatBound   f1 f2
  (StringBound  s1, StringBound  s2)       -> mergeBound String  StringBound  s1 s2
  (BytesBound   b1, BytesBound   b2)       -> mergeBound Bytes   BytesBound   b1 b2

  _                                        -> nope
  where
    nope = undefined -- conflicting d1 d2

    checkI = checkWith unreachable
    checkF = checkWith unreachable
    checkS = checkWith reMatch
    checkB = checkWith unreachable

    checkWith matchR x Bound {..} = do
      case _above of
        Open        -> pure ()
        Inclusive a -> when (x <  a) $ undefined
        Exclusive a -> when (x <= a) $ undefined
      case _below of
        Open        -> pure ()
        Inclusive b -> when (x >  b) $ undefined
        Exclusive b -> when (x >= b) $ undefined
      ar <- traverse (matchR x) _matchesAll
      nr <- traverse (matchR x) _matchesNone
      unless (and ar) $ undefined
      when   (or  nr) $ undefined
      when (any (x ==) _different) $ undefined

    mergeBound mkAtom mkBound b1 b2 = do
      let a = mergeEndpoint (>=) (b1 ^. above, b2 ^. above)
          b = mergeEndpoint (<=) (b1 ^. below, b2 ^. below)
          base = unbound
            & different   .~ nub (_different   b1 <> _different   b2)
            & matchesAll  .~ nub (_matchesAll  b1 <> _matchesAll  b2)
            & matchesNone .~ nub (_matchesNone b1 <> _matchesNone b2)
      case (a, b) of
        (Inclusive x, Inclusive y)
          | x >  y -> undefined -- FIXME
          | x == y -> pure $ Atom $ mkAtom x
        (Exclusive x, Exclusive y)
          | x >= y -> undefined -- FIXME
        (Inclusive x, Exclusive y)
          | x >= y -> undefined -- FIXME
        (Exclusive x, Inclusive y)
          | x >= y -> undefined -- FIXME
        _ ->
          pure $ mkBound $ base & above .~ a & below .~ b

    mergeEndpoint op = \case
      (Open, e) -> e
      (e, Open) -> e
      (Inclusive a, Inclusive b) -> if a `op` b then Inclusive a else Inclusive b
      (Exclusive a, Exclusive b) -> if a `op` b then Exclusive a else Exclusive b
      (Inclusive i, Exclusive e) -> if e `op` i then Exclusive e else Inclusive i
      (Exclusive e, Inclusive i) -> if e `op` i then Exclusive e else Inclusive i

evalList :: Seq Embedding -> Eval s (Seq Document)
evalList = traverse evalToNF . join <=< traverse retrieveEmbeddingThunks

retrieveEmbeddingThunks :: Applicative t => Embedding -> Eval s (t Thunk)
retrieveEmbeddingThunks = \case
  InlineThunk      t -> pure $ pure t
  Comprehension cs b -> undefined cs b

evalOr = curry \case
  (Atom (Boolean l), Atom (Boolean r)) -> pure $ Atom $ Boolean $ l || r
  (l, r)                               -> typeMismatch2 "||" l r

evalAnd = curry \case
  (Atom (Boolean l), Atom (Boolean r)) -> pure $ Atom $ Boolean $ l && r
  (l, r)                               -> typeMismatch2 "&&" l r

evalAdd = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Integer $ a + b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Float   $ fromInteger a + b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Float   $ a + fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Float   $ a + b
  (Atom (String  a), Atom (String  b)) -> pure $ Atom $ String  $ a <> b
  (Atom (Bytes   a), Atom (Bytes   b)) -> pure $ Atom $ Bytes   $ a <> b
  (l, r)                               -> typeMismatch2 "+" l r

evalSub = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Integer $ a - b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Float   $ fromInteger a - b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Float   $ a - fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Float   $ a - b
  (l, r)                               -> typeMismatch2 "-" l r

evalMul = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Integer $ a * b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Float   $ fromInteger a * b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Float   $ a * fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Float   $ a * b
  (Atom (Integer n), Atom (String  s)) -> pure $ Atom $ String  $ T.replicate (fromInteger n) s
  (Atom (String  s), Atom (Integer n)) -> pure $ Atom $ String  $ T.replicate (fromInteger n) s
  (Atom (Integer n), Atom (Bytes   s)) -> pure $ Atom $ Bytes   $ T.replicate (fromInteger n) s
  (Atom (Bytes   s), Atom (Integer n)) -> pure $ Atom $ Bytes   $ T.replicate (fromInteger n) s
  (l, r)                               -> typeMismatch2 "*" l r

evalDiv = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Float   $ fromInteger a / fromInteger b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Float   $ fromInteger a /             b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Float   $             a / fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Float   $             a /             b
  (l, r)                               -> typeMismatch2 "/" l r

evalEq = curry \case
  (Atom Null       , Atom Null       ) -> pure $ Atom $ Boolean True
  (Atom _          , Atom Null       ) -> pure $ Atom $ Boolean False
  (Atom Null       , Atom _          ) -> pure $ Atom $ Boolean False
  (Atom (Boolean l), Atom (Boolean r)) -> pure $ Atom $ Boolean $ l == r
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a == b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Boolean $ fromInteger a == b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a == fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Boolean $ a == b
  (Atom (String  a), Atom (String  b)) -> pure $ Atom $ Boolean $ a == b
  (Atom (Bytes   a), Atom (Bytes   b)) -> pure $ Atom $ Boolean $ a == b
  (l, r)                               -> typeMismatch2 "==" l r

evalNEq = curry \case
  (Atom Null       , Atom Null       ) -> pure $ Atom $ Boolean False
  (Atom _          , Atom Null       ) -> pure $ Atom $ Boolean True
  (Atom Null       , Atom _          ) -> pure $ Atom $ Boolean True
  (Atom (Boolean l), Atom (Boolean r)) -> pure $ Atom $ Boolean $ l /= r
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a /= b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Boolean $ fromInteger a /= b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a /= fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Boolean $ a /= b
  (Atom (String  a), Atom (String  b)) -> pure $ Atom $ Boolean $ a /= b
  (Atom (Bytes   a), Atom (Bytes   b)) -> pure $ Atom $ Boolean $ a /= b
  (l, r)                               -> typeMismatch2 "!=" l r

evalRE = curry \case
  (Atom (String  s), Atom (String  p)) -> Atom . Boolean <$> reMatch s p
  (Atom (String  s), Atom (Bytes   p)) -> Atom . Boolean <$> reMatch s p
  (l, r)                               -> typeMismatch2 "=~" l r

evalNRE = curry \case
  (Atom (String  s), Atom (String  p)) -> Atom . Boolean . not <$> reMatch s p
  (Atom (String  s), Atom (Bytes   p)) -> Atom . Boolean . not <$> reMatch s p
  (l, r)                               -> typeMismatch2 "!~" l r

evalLT = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a < b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Boolean $ fromInteger a < b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a < fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Boolean $ a < b
  (Atom (String  a), Atom (String  b)) -> pure $ Atom $ Boolean $ a < b
  (Atom (Bytes   a), Atom (Bytes   b)) -> pure $ Atom $ Boolean $ a < b
  (l, r)                               -> typeMismatch2 "<" l r

evalLE = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a <= b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Boolean $ fromInteger a <= b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a <= fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Boolean $ a <= b
  (Atom (String  a), Atom (String  b)) -> pure $ Atom $ Boolean $ a <= b
  (Atom (Bytes   a), Atom (Bytes   b)) -> pure $ Atom $ Boolean $ a <= b
  (l, r)                               -> typeMismatch2 "<=" l r

evalGT = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a > b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Boolean $ fromInteger a > b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a > fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Boolean $ a > b
  (Atom (String  a), Atom (String  b)) -> pure $ Atom $ Boolean $ a > b
  (Atom (Bytes   a), Atom (Bytes   b)) -> pure $ Atom $ Boolean $ a > b
  (l, r)                               -> typeMismatch2 ">" l r

evalGE = curry \case
  (Atom (Integer a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a >= b
  (Atom (Integer a), Atom (Float   b)) -> pure $ Atom $ Boolean $ fromInteger a >= b
  (Atom (Float   a), Atom (Integer b)) -> pure $ Atom $ Boolean $ a >= fromInteger b
  (Atom (Float   a), Atom (Float   b)) -> pure $ Atom $ Boolean $ a >= b
  (Atom (String  a), Atom (String  b)) -> pure $ Atom $ Boolean $ a >= b
  (Atom (Bytes   a), Atom (Bytes   b)) -> pure $ Atom $ Boolean $ a >= b
  (l, r)                               -> typeMismatch2 ">=" l r

evalPlus t = case t of
  Atom (Integer _) -> pure t
  Atom (Float   _) -> pure t
  _                -> typeMismatch "+" t

evalNeg = \case
  Atom (Integer a) -> pure $ Atom $ Integer $ negate a
  Atom (Float   a) -> pure $ Atom $ Float   $ negate a
  t                -> typeMismatch "-" t

evalNot = \case
  Atom (Boolean b) -> pure $ Atom $ Boolean $ not b
  t                -> typeMismatch "!" t

evalUNE = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & different %~ (x:)
  Atom (Float   x) -> pure $ FloatBound   $ unbound & different %~ (x:)
  Atom (String  x) -> pure $ StringBound  $ unbound & different %~ (x:)
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & different %~ (x:)
  Atom Null        -> pure NotNull
  t                -> typeMismatch "!=" t

evalURE = \case
  Atom (String  p) -> pure $ StringBound  $ unbound & matchesAll %~ (p:)
  Atom (Bytes   p) -> pure $ StringBound  $ unbound & matchesAll %~ (p:)
  t                -> typeMismatch "=~" t

evalUNRE = \case
  Atom (String  p) -> pure $ StringBound  $ unbound & matchesNone %~ (p:)
  Atom (Bytes   p) -> pure $ StringBound  $ unbound & matchesNone %~ (p:)
  t                -> typeMismatch "!~" t

evalULT = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & below .~ Exclusive x
  Atom (Float   x) -> pure $ FloatBound   $ unbound & below .~ Exclusive x
  Atom (String  x) -> pure $ StringBound  $ unbound & below .~ Exclusive x
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & below .~ Exclusive x
  t                -> typeMismatch "<" t

evalULE = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & below .~ Inclusive x
  Atom (Float   x) -> pure $ FloatBound   $ unbound & below .~ Inclusive x
  Atom (String  x) -> pure $ StringBound  $ unbound & below .~ Inclusive x
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & below .~ Inclusive x
  t                -> typeMismatch "<=" t

evalUGT = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & above .~ Exclusive x
  Atom (Float   x) -> pure $ FloatBound   $ unbound & above .~ Exclusive x
  Atom (String  x) -> pure $ StringBound  $ unbound & above .~ Exclusive x
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & above .~ Exclusive x
  t                -> typeMismatch ">" t

evalUGE = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & above .~ Inclusive x
  Atom (Float   x) -> pure $ FloatBound   $ unbound & above .~ Inclusive x
  Atom (String  x) -> pure $ StringBound  $ unbound & above .~ Inclusive x
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & above .~ Inclusive x
  t                -> typeMismatch ">=" t


--------------------------------------------------------------------------------
-- * Path substitution

-- | Subtitues paths in a given thunk.
--
-- When inlining an expresion, we need to alter the references within it to
-- point to the newly created value, to allow for proper overloading. Consider
-- the following example:
--
--     #a: {
--       b: number
--       c: b + 1
--     }
--     f: #a & {
--       b: 0
--     }
--
-- Internally, the @b@ reference in @#a.c@ is represented as an absolute path
-- @#a->b@. When substituting @#a@ in @f@, however, we must alter that path to
-- point to @f->b@; if we don't, we get something equivalent to:
--
--     f: {
--       b: 0
--       c: #a->b + 1 // whoops!
--     }
--
-- What we must do when inlining @#a@ is alter all of its inner references (any
-- reference that has an absolute path that starts with @#a@) to make it point
-- to the new path instead. What we want in the end is for @f@ to be akin to:
--
--     f: {
--       b: 0
--       c: f->b + 1
--     }
--
-- This function traverses the given thunk, and perfom path substitution on all
-- references that match the given prefix.
substitutePaths
  :: Path -- ^ original path
  -> Path -- ^ new path
  -> Thunk
  -> Thunk
substitutePaths old new = transform $ _Ref %~ modifyPath
  where
    modifyPath path = sub path (old, path)
    sub p = \case
      -- end cases
      (_,     Empty ) -> p
      (Empty, suffix) -> new <> suffix
      -- compare pair-wise
      (f1 :<| fs1, f2 :<| fs2)
        | f1 == f2  -> sub p (fs1, fs2)
        | otherwise -> p


--------------------------------------------------------------------------------
-- * Alias inlining

-- | Aliases can never be overloaded, and an alias and a field with the same
-- name cannot co-exist. Consequently, every occurence of a reference to an
-- alias can be replaced by the thunk associated with the alias or a field
-- reference to the field it points to.
--
-- Aliases in let clauses are a bit more strict wrt. cycles than fields or other
-- aliases are: if an alias appears within its own definition, inlining fails,
-- even if the definitions would be valid recursive fields.
--
-- For instance, this is valid:
--
--     let a = b
--     b = v: {c:0, r: a.c}
--
-- but this isn't:
--
--     let a = b
--     let b = {c:0, r: a.c}
--
-- the reason for this is that fields are *lazy*, and can therefore be mutually
-- recursive, as long as they can ultimately be resolved, as in the example
-- above. All aliases except for let clauses can be replaced by a reference to a
-- field:
--
--     a = v: 42   // a ref to @a@ can be replaced with an abolute ref to @v@
--     v: a = 42   // a ref to @a@ can be replaced with an abolute ref to @v@
--     let u = a   // cannot be replaced with a field, has to be inlined
--
-- in practice that means that we are delegating the task of detecting cycles to
-- the evaluation, since most alias inlining just replaces them with a field
-- reference.
--
-- However, in the case of let clauses, we can't rely on field laziness, since
-- there is no field to refer to, we therefore have to inline the full
-- expression everywhere. In theory, this could still potentially result in
-- something valid, such as the following:
--
--     // infinite list of zeroes
--     let zeroes = {car: 0, cdr: zs}
--
--     // let clauses' names aren't visible in their own scope
--     let zs = zeroes
--
--     // res is just one 0
--     res: zeroes.cdr.cdr.car
--
-- In practice, this doesn't work even if we make zeroes a field, since zeroes
-- itself would not be representable; but in theory this could be resolved with
-- laziness, but the language rejects it. The main reason (i suspect) is that
-- not only we can never remove the alias, but also that we would need more than
-- one inlining to be able to compute the result. With other aliases, after one
-- inlining, the IR contains a field reference, but no longer an alias
-- reference. But in this case, after inlining @zs@ then @zeroes@, we get:
--
--     let zeroes = {car: 0, cdr: {car: 0, cdr: zeroes}}
--     res: u.cdr.cdr.car
--
-- @zeroes@ still contains an alias reference to itself, and furthermore we
-- would need a second inlining of it to be able to compute `res`.
--
-- For simplicity, the language simply rejects this. The way it does is simple:
-- it checks whether the alias refers to itself. Here, whether we inline @zs@ or
-- @zeroes@ first, we get the other one to contain a reference to itself:
--
--     // inlining zs first
--     let zeroes = {car: 0, cdr: zeroes}
--
--     // inlining zeroes first
--     let zs = {car: 0, cdr: zs}
--
-- Furthermore, this is a "compilation error", not a bottom: finding such a
-- cycle makes the entire computation fail. This behaviour is not documented
-- explicitly, but modelled after the behaviour of the playground.
--
-- This function traverses the thunk hierarchy; and for each alias in each block
-- replaces all downstrean uses of that alias. On success, the output is
-- guaranteed not to contain any alias anymore.
--
-- WARNING: this is probably exponential: for each block we encounter we start
-- several traversals of some of the underlying thunks. Can we rewrite this as
-- one traversal, or does this become complicated wrt. mutually recursive let
-- clauses?
inlineAliases :: Thunk -> Either Errors Thunk
inlineAliases = itransformM go Empty
  where
    go absolutePath = _Block \b@BlockInfo {..} -> do
      let
        updateIdentFields  = M.mapWithKey \l ->
          fmap $ inlineFieldAlias (absolutePath :|> PathField       l)
        updateStringFields = M.mapWithKey \i ->
          fmap $ inlineFieldAlias (absolutePath :|> PathStringField i)
        updatedBlock = b
          & biIdentFields  %~ updateIdentFields
          & biStringFields %~ updateStringFields
      foldM (inlineBlockAlias absolutePath) updatedBlock $ M.keys _biAliases

-- | Inline a field alias, if any.
--
-- This function also removes the mention of the alias from the IR altogether.
inlineFieldAlias :: Path -> I.Field -> I.Field
inlineFieldAlias path f = case fieldAlias f of
  Nothing   -> f
  Just name -> f { fieldAlias = Nothing }
    & thunks %~ inlineInThunk path name (Ref path) path

-- | Given an alias appearing in a block, replace it in all other expressions.
--
-- This function also removes the alias from the block's definition.
inlineBlockAlias :: Path -> BlockInfo -> FieldLabel -> Either Errors BlockInfo
inlineBlockAlias blockPath b@BlockInfo {..} alias = do
  let
    (pathElem, thunk) = _biAliases M.! alias
    path = blockPath :|> pathElem
  -- check whether it appears in its own definition
  errorOnCycle path alias thunk
  -- then delete the current alias and update all thunks
  pure $ b
    & biAliases %~ sans alias
    & indexedThunks blockPath %@~ inlineInThunk path alias thunk

inlineInThunk :: Path -> FieldLabel -> Thunk -> Path -> Thunk -> Thunk
inlineInThunk varPath name new = itransform go
  where
    go l = \case
      Alias p n | varPath == p, name == n -> substitutePaths p l new
      t -> t

errorOnCycle :: Path -> FieldLabel -> Thunk -> Either Errors ()
errorOnCycle path name = void . transformM \case
  Alias p n | path == p, name == n -> Left (error "cycle!!!")
  t -> Right t


--------------------------------------------------------------------------------
-- * Error handling

report :: BottomSource -> Eval s a
report = throwError . EvaluationFailed . pure

typeMismatch :: String -> Document -> Eval s a
typeMismatch = undefined

typeMismatch2  :: String -> Document -> Document -> Eval s a
typeMismatch2 = undefined

orRecoverWith :: (Thunk -> Eval s Document) -> Thunk -> Eval s Document
orRecoverWith action thunk = action thunk `catchError` \case
  CannotBeEvaluatedYet -> pure $ Thunk thunk
  err                  -> throwError err

catchBottom :: Eval s Document -> Eval s (Either (Seq BottomSource) Document)
catchBottom action = fmap Right action `catchError` \case
  EvaluationFailed errs -> pure $ Left errs
  err                   -> throwError err


--------------------------------------------------------------------------------
-- * Helpers

reMatch :: Text -> Text -> Eval s Bool
reMatch str pat = do
  -- RE2 expects UTF-8 by default, and that's fine
  compiled <- RE2.compile (encodeUtf8 pat)
    `onLeft` \e -> error (RE2.errorMessage e)
  pure $ isJust $ RE2.find compiled (encodeUtf8 str)

newtype Mergeable a = Mergeable a
  deriving Functor

instance Eq (Mergeable (Document' Mergeable)) where
  Mergeable d1 == Mergeable d2 = case (d1, d2) of
    (NotNull       , NotNull       ) -> True
    (Atom         x, Atom         y) -> x == y
    (IntegerBound x, IntegerBound y) -> x == y
    (FloatBound   x, FloatBound   y) -> x == y
    (StringBound  x, StringBound  y) -> x == y
    (BytesBound   x, BytesBound   y) -> x == y
    (D.List       x, D.List       y) -> x == y
    -- WARNING: we ignore attributes for the purpose of merging
    -- similar values in a disjunction, just like te playground does
    (Struct (StructInfo fs1 _), Struct (StructInfo fs2 _)) ->
      fmap D.fieldValue fs1 == fmap D.fieldValue fs2
    _ -> False
