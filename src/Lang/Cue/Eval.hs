{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}

module Lang.Cue.Eval where

{-
  ( inlineAliases
  , eval
  ) where
-}

import "this" Prelude

import Control.Lens          hiding (Empty, List, below, op, re, (|>))
import Data.HashMap.Strict   qualified as M
import Data.List             (nub)
import Data.Sequence         as S
import Data.Text             qualified as T
import Data.Text.Encoding    (encodeUtf8)
import Regex.RE2             qualified as RE2

import Lang.Cue.AST          qualified as A
import Lang.Cue.Error
import Lang.Cue.Inline
import Lang.Cue.Internal.HKD
import Lang.Cue.IR           as I
import Lang.Cue.Location
import Lang.Cue.Value        as V

import Debug.Trace


--------------------------------------------------------------------------------
-- * Evaluation monad

newtype Eval a = Eval
  { runEval :: ReaderT Scope (Except EvalError) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Scope
    , MonadError EvalError
    , MonadFix
    )

data Scope = Scope
  { _currentPath :: Path
  , _knownThunks :: HashMap Path Thunk
  }
  deriving Show

data EvalError
  = CannotBeEvaluatedYet
  | EvaluationFailed (Seq BottomSource)
  deriving Show

makeLenses ''Scope

eval :: Thunk -> Either Errors Value
eval rootToken = do
  inlinedRoot <- inlineAliases rootToken
  evalToNF (Thunk inlinedRoot)
    & runEval
    & flip runReaderT (Scope [] mempty)
    & runExcept
    & translateError inlinedRoot
  where
    translateError t = \case
      Left (EvaluationFailed b) ->
        Left  $ fmap (withLocation (Location "" [] 0) . BottomError) b
      Left CannotBeEvaluatedYet ->
        Right $ Thunk t
      Right d ->
        Right d

debug :: Eval a -> Either EvalError a
debug x = x
  & runEval
  & flip runReaderT (Scope [] mempty)
  & runExcept


--------------------------------------------------------------------------------
-- * Scope manipulation

-- | Pushes a new field on the current path for the duration of the given
-- action. The field is "popped back" when the action terminates due to the use
-- of 'local'.
withPath :: PathElem -> Eval a -> Eval a
withPath label action = do
  parent <- view currentPath
  local (currentPath .~ (parent :|> label)) action

retrieveThunk :: Path -> Eval Thunk
retrieveThunk originalPath = do
  newPath <- asks _currentPath
  thunk <- asks (M.lookup originalPath . _knownThunks)
    `onNothingM` do traceShowM originalPath
                    traceShowM newPath
                    traceShowM =<< ask
                    panic ThunkNotFound
  pure $ substitutePaths originalPath newPath thunk


--------------------------------------------------------------------------------
-- * Evaluation

resolve :: Thunk -> Eval WHNFValue
resolve = evalToWHNF >=> collapse

collapse :: Value' f -> Eval (Value' f)
collapse = \case
  Disjoint    Empty    Empty -> unreachable
  Disjoint (Lone x)    Empty -> pure x
  Disjoint        _    Empty -> throwError CannotBeEvaluatedYet
  Disjoint        _ (Lone x) -> pure x
  Disjoint        _        _ -> throwError CannotBeEvaluatedYet
  d -> pure d

evalToNF :: WHNFValue -> Eval Value
evalToNF = \case
  Disjoint v d   -> nfDisjunction v d
  Struct s       -> nfStruct s
  V.List l       -> nfList l
  Thunk t        -> (evalToWHNF >=> evalToNF) `orRecoverWith` t
  V.Top          -> pure V.Top
  NotNull        -> pure NotNull
  Atom         x -> pure $ Atom         x
  V.Func       x -> pure $ V.Func       x
  V.Type       x -> pure $ V.Type       x
  IntegerBound x -> pure $ IntegerBound x
  FloatBound   x -> pure $ FloatBound   x
  StringBound  x -> pure $ StringBound  x
  BytesBound   x -> pure $ BytesBound   x

evalToWHNF :: Thunk -> Eval WHNFValue
evalToWHNF t = case t of
  Disjunction        d -> whnfDisjunction d
  Unification        u -> evalUnification u
  LogicalOr        l r -> join $ evalOr   <$> resolve l <*> resolve r
  LogicalAnd       l r -> join $ evalAnd  <$> resolve l <*> resolve r
  Equal            l r -> join $ evalEq   <$> resolve l <*> resolve r
  NotEqual         l r -> join $ evalNEq  <$> resolve l <*> resolve r
  Match            l r -> join $ evalRE   <$> resolve l <*> resolve r
  NotMatch         l r -> join $ evalNRE  <$> resolve l <*> resolve r
  LessThan         l r -> join $ evalLT   <$> resolve l <*> resolve r
  LessOrEqual      l r -> join $ evalLE   <$> resolve l <*> resolve r
  GreaterThan      l r -> join $ evalGT   <$> resolve l <*> resolve r
  GreaterOrEqual   l r -> join $ evalGE   <$> resolve l <*> resolve r
  Addition         l r -> join $ evalAdd  <$> resolve l <*> resolve r
  Subtraction      l r -> join $ evalSub  <$> resolve l <*> resolve r
  Multiplication   l r -> join $ evalMul  <$> resolve l <*> resolve r
  Division         l r -> join $ evalDiv  <$> resolve l <*> resolve r
  NumId              n -> evalPlus =<< resolve n
  Negate             n -> evalNeg  =<< resolve n
  LogicalNot         n -> evalNot  =<< resolve n
  IsNotEqualTo       n -> evalUNE  =<< resolve n
  Matches            n -> evalURE  =<< resolve n
  Doesn'tMatch       n -> evalUNRE =<< resolve n
  IsLessThan         n -> evalULT  =<< resolve n
  IsLessOrEqualTo    n -> evalULE  =<< resolve n
  IsGreaterThan      n -> evalUGT  =<< resolve n
  IsGreaterOrEqualTo n -> evalUGE  =<< resolve n
  Select v s           -> join $ evalSelect <$> evalToWHNF v <*> pure s
  Index  v i           -> join $ evalIndex  <$> evalToWHNF v <*> resolve i
  Slice  v b e         -> join $ evalSlice  <$> evalToWHNF v <*> traverse resolve b <*> traverse resolve e
  Call c a             -> evalCall c a
  I.List li            -> whnfList li
  Block s              -> whnfStruct s
  Interpolation _ _ts  -> undefined -- T.concat <$> traverse evalToString ts
  Ref path             -> evalRef path
  Alias _ _            -> error "unevaluated alias"
  Leaf a               -> pure $ Atom a
  I.Type c             -> pure $ V.Type c
  I.Func f             -> pure $ V.Func f
  I.Top                -> pure V.Top
  Bottom               -> report ArisedFromLiteral


--------------------------------------------------------------------------------
-- * Disjunction

whnfDisjunction :: Seq (Bool, Thunk) -> Eval WHNFValue
whnfDisjunction disj = mdo
  (result, starred, errors, marked, hasUnevaluated) <-
    disj & flip foldlM (S.empty, S.empty, S.empty, False, False)
      \(r, s, e, m, u) (star, thunk) -> do
        mr <- fmap Right (evalToWHNF thunk) `catchError` (pure . Left)
        pure $ case mr of
          Left CannotBeEvaluatedYet   -> (r, s, e,        m || star, True)
          Left (EvaluationFailed err) -> (r, s, e <> err, m || star,    u)
          Right doc ->
            let (val, def) = case doc of
                  Disjoint v Empty -> (v, if star then v else Empty)
                  Disjoint v d     -> (v, if star || not marked then d else Empty)
                  _                -> (pure doc, if star then pure doc else Empty)
            in  (r <> val, s <> def, e, m || star, u)
  when (S.null result) $
    throwError $
      if hasUnevaluated
      then CannotBeEvaluatedYet
      else EvaluationFailed errors
  pure $ case (result, starred) of
    (Lone x, Empty) -> x
    _               -> Disjoint result starred

nfDisjunction :: Seq WHNFValue -> Seq WHNFValue -> Eval Value
nfDisjunction = disjunctionWith evalToNF mergeable

disjunctionWith
  :: (a -> Eval (Value' f))
  -> (Value' f -> Value' f -> Bool)
  -> Seq a
  -> Seq a
  -> Eval (Value' f)
disjunctionWith f equal dv dd = do
  (vs, es, u) <- foldlM step (Empty, Empty, False) dv
  (ds,  _, _) <- foldlM step (Empty, Empty, False) dd
  when (S.null vs) $
    throwError $ if u then CannotBeEvaluatedYet else EvaluationFailed es
  pure case (nubBy equal vs, nubBy equal ds) of
    (Lone x, Empty) -> x
    (v, d)          -> Disjoint v d
  where
    step (vs, es, u) w =
      fmap Right (f w) `catchError` (pure . Left) <&> \case
        Left CannotBeEvaluatedYet -> (vs, es, True)
        Left (EvaluationFailed e) -> (vs, es <> e, u)
        Right v                   -> (vs :|> v, es, u)

mergeable :: Value -> Value -> Bool
mergeable = (==) `on` (Mergeable . abstract Mergeable)

newtype Mergeable a = Mergeable a
  deriving Functor

instance Eq (Mergeable (Value' Mergeable)) where
  Mergeable d1 == Mergeable d2 = case (d1, d2) of
    (V.Top         , V.Top         ) -> True
    (NotNull       , NotNull       ) -> True
    (Atom         x, Atom         y) -> x == y
    (IntegerBound x, IntegerBound y) -> x == y
    (FloatBound   x, FloatBound   y) -> x == y
    (StringBound  x, StringBound  y) -> x == y
    (BytesBound   x, BytesBound   y) -> x == y
    (V.List       x, V.List       y) -> x == y
    (Thunk        x, Thunk        y) -> x == y
    -- WARNING: we ignore attributes for the purpose of merging
    -- similar values in a disjunction, just like te playground does
    (Struct s1, Struct s2) -> and @[]
      [ _sFields      s1 == _sFields      s2
      , _sConstraints s1 == _sConstraints s2
      , _sClosed      s1 == _sClosed      s2
      ]
    _ -> False


--------------------------------------------------------------------------------
-- * Unification

evalUnification :: Seq Thunk -> Eval WHNFValue
evalUnification = foldlM unify V.Top <=< traverse evalToWHNF

unify :: WHNFValue -> WHNFValue -> Eval WHNFValue
unify d1 d2 = case (d1, d2) of
  (Disjoint v1 s1, Disjoint v2 s2)                 -> distribute  v1   s1   v2   s2
  (Disjoint v1 s1,              _)                 -> distribute  v1   s1  [d2] [d2]
  (             _, Disjoint v2 s2)                 -> distribute [d1] [d1]  v2   s2

  (Thunk (Unification u1), Thunk (Unification u2)) -> pure $ Thunk $ Unification $ u1  <> u2
  (Thunk (Unification u1), Thunk t2              ) -> pure $ Thunk $ Unification $ u1 :|> t2
  (Thunk t1              , Thunk (Unification u2)) -> pure $ Thunk $ Unification $ t1 :<| u2
  (Thunk t1              , Thunk t2              ) -> pure $ Thunk $ Unification [t1, t2]
  (_                     , Thunk t2              ) -> evalToWHNF t2 >>= \x -> unify d1 x
  (Thunk t1              , _                     ) -> evalToWHNF t1 >>= \x -> unify x d2

  (V.Top, _)                                       -> pure d2
  (_, V.Top)                                       -> pure d1

  (NotNull, NotNull)                               -> pure NotNull

  (V.List l1, V.List l2)                           -> unifyLists l1 l2
  (  NotNull, V.List  _)                           -> pure d2
  (V.List  _,   NotNull)                           -> pure d1
  (Struct s1, Struct s2)                           -> unifyStructs s1 s2
  (  NotNull, Struct  _)                           -> pure d2
  (Struct  _,   NotNull)                           -> pure d1

  (Atom   a1, Atom   a2) | a1 == a2                -> pure d1
  (Atom   a1, NotNull  ) | a1 /= Null              -> pure d1
  (NotNull,   Atom   a2) | a2 /= Null              -> pure d2

  (V.Type IntegerType, V.Type IntegerType)         -> pure d1
  (V.Type IntegerType, V.Type  NumberType)         -> pure d1
  (V.Type  NumberType, V.Type IntegerType)         -> pure d2
  (V.Type   FloatType, V.Type   FloatType)         -> pure d1
  (V.Type   FloatType, V.Type  NumberType)         -> pure d1
  (V.Type  NumberType, V.Type   FloatType)         -> pure d2
  (V.Type  NumberType, V.Type  NumberType)         -> pure d1
  (V.Type  StringType, V.Type  StringType)         -> pure d1
  (V.Type   BytesType, V.Type   BytesType)         -> pure d1
  (V.Type BooleanType, V.Type BooleanType)         -> pure d1

  (V.Type IntegerType, IntegerBound _)             -> pure d2
  (V.Type  NumberType, IntegerBound _)             -> pure d2
  (V.Type   FloatType,   FloatBound _)             -> pure d2
  (V.Type  NumberType,   FloatBound _)             -> pure d2
  (V.Type  StringType,  StringBound _)             -> pure d2
  (V.Type   BytesType,   BytesBound _)             -> pure d2
  (IntegerBound _, V.Type IntegerType)             -> pure d1
  (IntegerBound _, V.Type  NumberType)             -> pure d1
  (  FloatBound _, V.Type   FloatType)             -> pure d1
  (  FloatBound _, V.Type  NumberType)             -> pure d1
  ( StringBound _, V.Type  StringType)             -> pure d1
  (  BytesBound _, V.Type   BytesType)             -> pure d1

  (Atom (Integer x), IntegerBound b)               -> d1 <$ checkWith unreachable x b
  (Atom (Float   x), FloatBound   b)               -> d1 <$ checkWith unreachable x b
  (Atom (String  x), StringBound  b)               -> d1 <$ checkWith reMatch     x b
  (Atom (Bytes   x), BytesBound   b)               -> d1 <$ checkWith unreachable x b
  (IntegerBound b, Atom (Integer x))               -> d2 <$ checkWith unreachable x b
  (FloatBound   b, Atom (Float   x))               -> d2 <$ checkWith unreachable x b
  (StringBound  b, Atom (String  x))               -> d2 <$ checkWith reMatch     x b
  (BytesBound   b, Atom (Bytes   x))               -> d2 <$ checkWith unreachable x b

  (Atom (Boolean _), V.Type BooleanType)           -> pure d1
  (Atom (Integer _), V.Type IntegerType)           -> pure d1
  (Atom (Integer _), V.Type  NumberType)           -> pure d1
  (Atom (Float   _), V.Type   FloatType)           -> pure d1
  (Atom (Float   _), V.Type  NumberType)           -> pure d1
  (Atom (String  _), V.Type  StringType)           -> pure d1
  (Atom (Bytes   _), V.Type   BytesType)           -> pure d1
  (V.Type BooleanType, Atom (Boolean _))           -> pure d2
  (V.Type IntegerType, Atom (Integer _))           -> pure d2
  (V.Type  NumberType, Atom (Integer _))           -> pure d2
  (V.Type   FloatType, Atom (Float   _))           -> pure d2
  (V.Type  NumberType, Atom (Float   _))           -> pure d2
  (V.Type  StringType, Atom (String  _))           -> pure d2
  (V.Type   BytesType, Atom (Bytes   _))           -> pure d2

  (IntegerBound i1, IntegerBound i2)               -> mergeBound Integer IntegerBound i1 i2
  (FloatBound   f1, FloatBound   f2)               -> mergeBound Float   FloatBound   f1 f2
  (StringBound  s1, StringBound  s2)               -> mergeBound String  StringBound  s1 s2
  (BytesBound   b1, BytesBound   b2)               -> mergeBound Bytes   BytesBound   b1 b2

  _                                                -> do
    traceM "CONFLICT"
    traceShowM d1
    traceShowM d2
    report ArisedFromLiteral
    -- undefined -- conflicting d1 d2
  where
    checkWith matchR x Bound {..} = do
      case _above of
        Open        -> pure ()
        Inclusive a -> when (x <  a) $ undefined
        Exclusive a -> when (x <= a) $ report ArisedFromLiteral
      case _below of
        Open        -> pure ()
        Inclusive b -> when (x >  b) $ undefined
        Exclusive b -> when (x >= b) $ report ArisedFromLiteral
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

unifies v1 v2 = (True <$ unify v1 v2) `catchError` const (pure False)

distribute :: Seq WHNFValue -> Seq WHNFValue -> Seq WHNFValue -> Seq WHNFValue -> Eval WHNFValue
distribute v1 d1 v2 d2 =
  disjunctionWith
    (uncurry unify)
    (\_ _ -> False)
    (liftA2 (,) v1 v2)
    (liftA2 (,) d1 d2)

unifyLists :: WHNFListInfo -> WHNFListInfo -> Eval WHNFValue
unifyLists (V.ListInfo l1 c1) (V.ListInfo l2 c2) = do
  let n1 = S.length l1
      n2 = S.length l2
  when (n1 < n2 && S.null c1) $ error "list length mismatch"
  when (n2 < n1 && S.null c2) $ error "list length mismatch"
  let c3 = c1 <> c2
      l3 = go l1 l2
  pure $ V.List $ V.ListInfo l3 c3
  where
    go :: Seq (Const Thunk WHNFValue) -> Seq (Const Thunk WHNFValue) -> Seq (Const Thunk WHNFValue)
    go Empty      Empty      = Empty
    go xs         Empty      = xs <&> \x -> Const $ Unification $ fmap getConst (x :<| c2)
    go Empty      ys         = ys <&> \y -> Const $ Unification $ fmap getConst (y :<| c1)
    go (x :<| xs) (y :<| ys) = Const (Unification [getConst x, getConst y]) :<| go xs ys

unifyConstraints :: Seq (WHNFValue, Const Thunk a) -> FieldLabel -> WHNFField -> Eval WHNFField
unifyConstraints constraints name = fValue \t -> foldlM step t constraints
  where
    nameAtom = Atom $ String $ labelName name
    step v (cond, expr) =
      unifies nameAtom cond <&> bool v
        -- TODO: MUST CORRECT PATHS
        -- TODO: replace reference to captured name by 'nameAtom'
        (Const $ Unification [getConst v, getConst expr])

unifyStructs :: WHNFStructInfo -> WHNFStructInfo -> Eval WHNFValue
unifyStructs s1 s2 = do
  f1s <- _sFields s1 & M.traverseWithKey \label field ->
    unifyConstraints (_sConstraints s2) label field
  f2s <- _sFields s2 & M.traverseWithKey \label field ->
    unifyConstraints (_sConstraints s1) label field
  pure $ Struct $ StructInfo
    { _sFields      = M.unionWith unifyFields f1s f2s
    , _sConstraints = _sConstraints s1 <> _sConstraints s2
    , _sAttributes  = M.unionWith (<>) (_sAttributes s1) (_sAttributes s2)
    , _sClosed      = _sClosed s1 || _sClosed s2 -- TODO: ?
    }
  where
    unifyFields f1 f2 = V.Field
      { _fValue      = Const $ Unification $ getConst <$> [_fValue f1, _fValue f2]
      , _fOptional   = _fOptional f1 <> _fOptional f2
      , _fAttributes = M.unionWith (<>) (_fAttributes f1) (_fAttributes f2)
      }


--------------------------------------------------------------------------------
-- * List and embeddings

whnfList :: I.ListInfo -> Eval WHNFValue
whnfList I.ListInfo {..} = do
  ts <- traverse evalEmbeddingThunks listElements
  pure $ V.List $ V.ListInfo
    { _lValues  = fmap Const $ join ts
    , _lDefault = fmap Const $ fromList $ toList listConstraint
    }

evalEmbeddingThunks :: Embedding -> Eval (Seq Thunk)
evalEmbeddingThunks = \case
  InlineThunk      t -> pure $ pure t
  Comprehension cs b -> undefined cs b

nfList :: WHNFListInfo -> Eval Value
nfList V.ListInfo {..} = do
  res <- traverse (evalToNF . Thunk . getConst) _lValues
  pure $ V.List $ V.ListInfo res []


--------------------------------------------------------------------------------
-- * Struct

whnfStruct :: BlockInfo -> Eval WHNFValue
whnfStruct BlockInfo {..} = do
  cs <- foldlM resolveConstraint Empty _biConstraints
  fs <- M.traverseWithKey (mergeFields cs) _biIdentFields
  pure $ V.Struct $ StructInfo fs cs _biAttributes False
  where
    -- | we can already resolve constraints' patterns: they are not allowed to
    -- refer to fields from within this block
    resolveConstraint res (pat, val) =
      do n <- evalToWHNF pat
         pure $ res :|> (n, Const val)
      `catchError` const (pure res)
    mergeFields cs label ts = do
      let (unifiedThunks, opts, attrs) = foldl step (Empty, A.Optional, M.empty) ts
          thunk = Unification unifiedThunks
          field = V.Field (Const thunk) opts attrs
      unifyConstraints cs label field
    step (ts, opt, attrs) I.Field {..} =
      ( ts :|> fieldValue
      , opt <> fieldOptional
      , M.unionWith (<>) attrs fieldAttributes
      )

nfStruct :: WHNFStructInfo -> Eval Value
nfStruct StructInfo {..} = do
  parent <- view currentPath
  let newMappings = M.toList _sFields <&> \(label, field) ->
        (parent :|> PathField label, getConst $ _fValue field)
  local (knownThunks %~ mappend (M.fromList newMappings)) do
    fs <- foldlM processField mempty $ M.toList _sFields
    pure $ Struct $ StructInfo fs [] _sAttributes _sClosed
  where
    processField res (label, V.Field {..})
      | _fOptional == A.Optional   = pure res
      | labelType label /= Regular = pure res
      | otherwise = withPath (PathField label) do
          result <- evalToNF $ Thunk $ getConst _fValue
          pure $ M.insert label (V.Field result _fOptional _fAttributes) res


--------------------------------------------------------------------------------
-- * Ref

evalRef :: Path -> Eval WHNFValue
evalRef path = do
  -- current <- view currentPath
  -- TODO: cycle detection goes here
  result <- evalToWHNF =<< retrieveThunk path
  pure $ case (path, result) of
           (_ :|> PathField f, Struct s) ->
             if I.labelType f `elem` closedFieldTypes
             then Struct $ s { _sClosed = True }
             else result
           _ -> result
  where
    closedFieldTypes :: [FieldType]
    closedFieldTypes = [Definition, HiddenDefinition]


--------------------------------------------------------------------------------
-- * Primary

evalSelect :: WHNFValue -> FieldLabel -> Eval WHNFValue
evalSelect v label = case v of
  Struct s -> do
    when (labelType label /= Regular) $
      throwError CannotBeEvaluatedYet
    fieldInfo <- M.lookup label (_sFields s)
      `onNothing` throwError CannotBeEvaluatedYet
    evalToWHNF $ getConst $ _fValue fieldInfo
  w -> typeMismatch "." w

evalIndex :: WHNFValue -> WHNFValue -> Eval WHNFValue
evalIndex = curry \case
  (V.List l, Atom (Integer i)) -> do
    r <- S.lookup (fromInteger i) (_lValues l)
      `onNothing` report undefined
    evalToWHNF $ getConst r
  (Struct s, Atom (String  i)) -> do
    fieldInfo <- M.lookup (I.FieldLabel i Regular) (_sFields s)
      `onNothing` report undefined
    evalToWHNF $ getConst $ _fValue fieldInfo
  (a, b) -> typeMismatch2 "[]" a b

evalSlice :: WHNFValue -> Maybe WHNFValue -> Maybe WHNFValue -> Eval WHNFValue
evalSlice v b e = case v of
  V.List (V.ListInfo l _) -> do
    let n = S.length l
    sb <- resolveInt b 0
    se <- resolveInt e n
    when (sb > se) $ report undefined
    when (sb <  0) $ report undefined
    when (se >  n) $ report undefined
    let res = S.take (se - sb) $ S.drop sb l
    pure $ V.List $ V.ListInfo res Empty
  _ -> report undefined
  where
    resolveInt x d = case x of
      Just (Atom (Integer i)) -> pure (fromInteger i)
      Nothing                 -> pure d
      _                       -> report undefined

evalCall :: Thunk -> [Thunk] -> Eval WHNFValue
evalCall fun args = evalToWHNF fun >>= \case
  V.Func (Function _ f) -> f =<< traverse evalToWHNF args
  _                   -> report undefined


--------------------------------------------------------------------------------
-- * Binary operators

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


--------------------------------------------------------------------------------
-- * Unary operators

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
  -- TODO: handle bounds
  t                -> typeMismatch "<" t

evalULE = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & below .~ Inclusive x
  Atom (Float   x) -> pure $ FloatBound   $ unbound & below .~ Inclusive x
  Atom (String  x) -> pure $ StringBound  $ unbound & below .~ Inclusive x
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & below .~ Inclusive x
  -- TODO: handle bounds
  t                -> typeMismatch "<=" t

evalUGT = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & above .~ Exclusive x
  Atom (Float   x) -> pure $ FloatBound   $ unbound & above .~ Exclusive x
  Atom (String  x) -> pure $ StringBound  $ unbound & above .~ Exclusive x
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & above .~ Exclusive x
  -- TODO: handle bounds
  t                -> typeMismatch ">" t

evalUGE = \case
  Atom (Integer x) -> pure $ IntegerBound $ unbound & above .~ Inclusive x
  Atom (Float   x) -> pure $ FloatBound   $ unbound & above .~ Inclusive x
  Atom (String  x) -> pure $ StringBound  $ unbound & above .~ Inclusive x
  Atom (Bytes   x) -> pure $ BytesBound   $ unbound & above .~ Inclusive x
  -- TODO: handle bounds
  t                -> typeMismatch ">=" t


--------------------------------------------------------------------------------
-- * Error handling

report :: BottomSource -> Eval a
report = throwError . EvaluationFailed . pure

typeMismatch :: String -> WHNFValue -> Eval a
typeMismatch _ _ = throwError CannotBeEvaluatedYet

typeMismatch2  :: String -> WHNFValue -> WHNFValue -> Eval a
typeMismatch2 _ _ _ = throwError CannotBeEvaluatedYet

orRecoverWith :: (Thunk -> Eval (Value' f)) -> Thunk -> Eval (Value' f)
orRecoverWith action thunk = action thunk `catchError` \case
  CannotBeEvaluatedYet -> pure $ Thunk thunk
  err                  -> throwError err

catchBottom :: Eval (Value' f) -> Eval (Either (Seq BottomSource) (Value' f))
catchBottom action = fmap Right action `catchError` \case
  EvaluationFailed errs -> pure $ Left errs
  err                   -> throwError err


--------------------------------------------------------------------------------
-- * Helpers

reMatch :: Text -> Text -> Eval Bool
reMatch str pat = do
  -- RE2 expects UTF-8 by default, and that's fine
  compiled <- RE2.compile (encodeUtf8 pat)
    `onLeft` \e -> error (RE2.errorMessage e)
  pure $ isJust $ RE2.find compiled (encodeUtf8 str)
