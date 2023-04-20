{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all code that translates from the AST into the IR.
module Lang.Cue.Translate
  ( translateFile
  , translateExpression
  ) where

import "this" Prelude

import Control.Lens           hiding (List, (|>))
import Control.Monad.Extra    (unlessM, whenJust)
import Control.Monad.Validate
import Data.Char
import Data.HashMap.Strict    qualified as M
import Data.HashSet           qualified as S
import Data.List              qualified as L
import Data.List.NonEmpty     as NE
import Data.Sequence          as Seq
import Data.Text              qualified as T

import Lang.Cue.AST           as A
import Lang.Cue.Builtins      qualified as F
import Lang.Cue.Error
import Lang.Cue.IR            as I
import Lang.Cue.Tokens


--------------------------------------------------------------------------------
-- * Translation context

newtype Translation a = Translation
  ( ReaderT Packages (StateT Scope (Validate Errors)) a
  )
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadReader Packages
    , MonadState Scope
    , MonadValidate Errors
    )

type Packages = HashMap Text Thunk

-- | Stores the current scope during the translation phase. For each kind of
-- bindings and for each name, we store a stack: on the way back up, we can pop
-- the values we added and restore the previous context.
data Scope = Scope
  { _currentPath    :: Path
  , _visibleFields  :: HashMap FieldLabel (NonEmpty Path)
  , -- | we keep an additional boolean to keep track of whether an alias has
    -- been used in its scope; it's an (undocumented) error to have an unused
    -- alias
    _visibleAliases :: HashMap FieldLabel (NonEmpty (Path, Bool))
  }

makeLenses ''Scope

runTranslation :: Packages -> Translation a -> Either Errors a
runTranslation pkgs (Translation a) = a
  & flip runReaderT pkgs
  & flip evalStateT (Scope mempty mempty mempty)
  & runValidate


--------------------------------------------------------------------------------
-- * API

translateFile
  :: HashMap Text Package
  -> SourceFile
  -> Either Errors Package
translateFile pkgs SourceFile {..} = do
  imported <- foldM (processImport pkgs) M.empty moduleImports
  topBlock <- runTranslation imported $ translateBlock moduleDeclarations
  pure Package
    { pkgName = maybe "" getIdentifier moduleName
    , pkgDocument = Block topBlock
    , pkgAttributes = translateAttributes moduleAttributes
    }

translateExpression
  :: Expression
  -> Either Errors Thunk
translateExpression = runTranslation mempty . translateExpr


--------------------------------------------------------------------------------
-- * Scope manipulation

-- | Pushes a new field on the current path for the duration of the given
-- action. The field is popped back when the action terminates.
--
-- To guarantee that we never fail to pop the path back, this function **catches
-- errors** arising from the use of 'refute', using 'tolerate'. This means that,
-- on error, we ignore the rest of the field, but attempt to resume translation
-- to collect as many errors as possible.
--
-- We don't attempt to catch actual exceptions with 'finally', since actual IO
-- exceptions are not used as a control mechanism throughout the translation.
withPath :: PathElem -> Translation a -> Translation (Maybe a)
withPath label action = do
  currentPath %= (|> label)
  result <- tolerate action
  currentPath %= \case
    upperPath :|> _ -> upperPath
    -- if we reach this, it means that the action erroneously modified the path
    _               -> unreachable
  pure result

-- | Add the absolute path of the given field to the current scope.
pushField :: FieldLabel -> Translation ()
pushField f = do
  parentPath <- use currentPath
  visibleFields %= M.insertWith (<>) f (pure $ parentPath :|> PathField f)

-- | Pop the given field.
popField :: FieldLabel -> Translation ()
popField f = visibleFields %= flip M.update f \(_ :| t) -> nonEmpty t

-- | Add the given alias to the current scope.
pushAlias :: (FieldLabel, Path) -> Translation ()
pushAlias = pushAliasWith False

-- | Add the given alias to the current scope at current path.
pushAliasAtCurrentPath :: FieldLabel -> Translation ()
pushAliasAtCurrentPath l = do
  parentPath <- use currentPath
  pushAliasWith False (l, parentPath)

-- | Add the given alias to the current scope, with the given used status.
pushAliasWith :: Bool -> (FieldLabel, Path) -> Translation ()
pushAliasWith b (l, p) =
  visibleAliases %= M.insertWith (<>) l (pure (p, b))

-- | Mark the given alias as used. This function does NOT check that the alias
-- exists.
useAlias :: FieldLabel -> Translation ()
useAlias l = visibleAliases . ix l . ix 0 . _2 .= True

-- | Removes the given alias from the stack, and returns whether it was used.
popAlias :: FieldLabel -> Translation (Path, Bool)
popAlias l =
  uses visibleAliases (M.lookup l) >>= \case
    -- if we reach this, it means that we tried to pop a non-existant alias
    Nothing -> unreachable
    Just (v :| r) -> do
      visibleAliases %= flip M.update l (const $ nonEmpty r)
      pure v

-- | Removes the given alias from the stack and raise a non-fatal error if it
-- wasn't used.
popAndCheckAlias :: FieldLabel -> Translation ()
popAndCheckAlias l =
  unlessM (snd <$> popAlias l) $
    dispute $ error "alias not used!" -- FIXME

-- | Pushes all the definition in a block onto the current scope, runs the given
-- action, then pops the newly added fields. This function will raise non-fatal
-- errors if it detects that an alias was popped unused (see 'Scope').
-- Like 'withPath', it 'tolerates' errors from the given action, to ensure we
-- end up with a correct scope.
withScope
  :: (HashSet FieldLabel, HashMap FieldLabel Path)
  -> Translation a
  -> Translation (Maybe a)
withScope (fields, aliases) action = do
  traverse_ pushAlias $ M.toList aliases
  traverse_ pushField fields
  result <- tolerate action
  traverse_ popField fields
  traverse_ popAndCheckAlias $ M.keys aliases
  pure result


--------------------------------------------------------------------------------
-- * Reference resolving

resolve :: Identifier -> Translation Thunk
resolve name = go
  [ resolveAlias
  , resolveField
  , resolvePackage
  , resolveBuiltin
  ]
  where
    go = \case
      (f:fs) -> f name `onNothingM` go fs
      []     -> refute (error "name not found")

resolveAlias :: Identifier -> Translation (Maybe Thunk)
resolveAlias name = do
  label <- translateIdentifier name
  uses visibleAliases (M.lookup label) >>=
    traverse \(fst . NE.head -> parentPath) -> do
      useAlias label
      pure $ Alias parentPath label

resolveField :: Identifier -> Translation (Maybe Thunk)
resolveField name = do
  label <- translateIdentifier name
  uses visibleFields (M.lookup label) <<&>> Ref . NE.head

resolvePackage :: Identifier -> Translation (Maybe Thunk)
resolvePackage name =
  asks (M.lookup (getIdentifier name))

resolveBuiltin :: Identifier -> Translation (Maybe Thunk)
resolveBuiltin = getIdentifier >>> pure. \case
  -- types
  "bool"    -> Just $ Type BooleanType
  "number"  -> Just $ Type NumberType
  "integer" -> Just $ Type IntegerType
  "float"   -> Just $ Type FloatType
  "string"  -> Just $ Type StringType
  "bytes"   -> Just $ Type BytesType

  -- functions
  "len"   -> Just $ Func F.len
  "close" -> Just $ Func undefined
  "and"   -> Just $ Func undefined
  "or"    -> Just $ Func undefined
  "div"   -> Just $ Func undefined
  "mod"   -> Just $ Func undefined
  "quot"  -> Just $ Func undefined
  "rem"   -> Just $ Func undefined

  -- misc
  "null" -> Just $ Leaf Null
  "_"    -> Just $ Top
  _      -> Nothing


--------------------------------------------------------------------------------
-- * Translation rules

translateBlock :: [Declaration] -> Translation BlockInfo
translateBlock decls = do
  scope <- get
  let startBlock = BlockInfo
        { _biAttributes   = M.empty
        , _biAliases      = M.empty
        , _biIdentFields  = M.empty
        , _biStringFields = Seq.empty
        , _biEmbeddings   = Seq.empty
        , _biConstraints  = Seq.empty
        , _biClosed       = False
        }
  -- we traverse all the declarations; doing so, we collect all the fields and
  -- aliases to add to the current scope, and the monadic actions to run in the
  -- update scope
  (blockFields, blockAliases, blockBuilder) <-
    foldM (translateDeclaration scope) (mempty, mempty, pure startBlock) decls
  block <- withScope (blockFields, blockAliases) blockBuilder
  pure $ fromMaybe startBlock block

translateDeclaration
  :: Scope
  -> ( HashSet FieldLabel
     , HashMap FieldLabel Path
     , Translation BlockInfo
     )
  -> Declaration
  -> Translation
       ( HashSet FieldLabel
       , HashMap FieldLabel Path
       , Translation BlockInfo
       )
translateDeclaration scope (fields, aliases, builder) = \case
  -- Attribute
  DeclarationAttribute Attribute {..} -> do
    let
      newBuilder = do
        block <- builder
        pure $
          block & biAttributes %~
            M.insertWith (flip (<>)) (getIdentifier attributeName) (pure attributeText)
    pure (fields, aliases, newBuilder)

  -- Let clause
  DeclarationLetClause LetClause {..} -> do
    fieldLabel <- translateIdentifier letName
    -- check for conflicts with the parent scope and with other definitions in
    -- this block
    when (fieldLabel `M.member` _visibleFields scope) $
      refute $ error "scope conflict between alias and field" -- FIXME
    when (fieldLabel `S.member` fields) $
      refute $ error "scope conflict between alias and field" -- FIXME
    when (fieldLabel `M.member` aliases) $
      refute $ error "alias defined more than once" -- FIXME
    let
      pathItem   = PathLetClause fieldLabel
      aliasPath  = _currentPath scope :|> pathItem
      newAliases = M.insert fieldLabel aliasPath aliases
      newBuilder = do
        block <- builder
        thunk <- recover $ fmap join $ withPath pathItem do
          -- WARNING: UNDOCUMENTED BEHAVIOUR
          -- special case for let clauses: we have to remove the let identifier
          -- itself from the scope, to prevent let clause self-recursion, and to
          -- prevent shadowing previous let clauses with the same name; this is
          -- undocumented behaviour, and it's unclear whether it is on purpose or
          -- not
          (path, wasUsed) <- popAlias fieldLabel
          result <- tolerate $ translateExpr letExpression
          pushAliasWith wasUsed (fieldLabel, path)
          pure result
        pure $ block
          & biAliases . at fieldLabel ?~ (pathItem, thunk)
    pure (fields, newAliases, newBuilder)

  -- Embedding
  DeclarationEmbedding (EmbeddedExpression AliasedExpression {..}) -> do
    -- WARNING: UNDOCUMENTED BEHAVIOUR
    -- the reference claims that an embed can be an aliased expression, but the
    -- playground rejects it. in doubt, we do the same
    whenJust aeAlias $ const $
      refute $ error "alias in embed" -- FIXME
    let
      newBuilder = do
        block <- builder
        thunk <- recover $ tolerate $ translateExpr aeExpression
        pure $ block & biEmbeddings %~ (:|> InlineThunk thunk)
    pure (fields, aliases, newBuilder)
  DeclarationEmbedding (EmbeddedComprehension A.Comprehension {..}) -> do
    let
      newBuilder = do
        block <- builder
        embed <- translateComprehension compClauses compResult
        pure $ block & biEmbeddings %~ (:|> embed)
    pure (fields, aliases, newBuilder)

  -- Ellipsis
  DeclarationEllipsis _ ->
    -- Not supported yet, as per the reference.
    refute $ error "ellipsis in struct" -- FIXME

  DeclarationField A.Field {..} -> do
    let Label {..} = fieldLabel
    case labelExpression of
      -- String field
      LabelString s opt -> do
        nameThunk <- translateStringLiteral (TextInfo SingleLineString 0) s
        let AliasedExpression {..} = fieldExpression
        -- check for conflicts from the field's alias (if any)
        fAlias <- for labelAlias \a -> do
          l <- translateIdentifier a
          when (l `M.member` _visibleFields scope) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `S.member` fields) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` aliases) $
            refute $ error "alias defined more than once" -- FIXME
          pure l
        -- check for conflicts from the expression's alias (if any)
        eAlias <- for aeAlias \a -> do
          l <- translateIdentifier a
          when (l `M.member` _visibleFields scope) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `S.member` fields) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` aliases) $
            refute $ error "alias defined more than once" -- FIXME
          pure l
        -- finally evaluate the expression
        let
          -- WARNING: BUG?
          -- the reference claims that the alias to an optional field is only visible
          -- within the definition of that field, but the playground disagrees; we make
          -- the alias visible to the whole block to be consistent
          pathItem   = PathStringField
          aliasPath  = _currentPath scope :|> pathItem
          newAliases = aliases & maybe id (flip M.insert aliasPath) fAlias
          newBuilder = do
            block <- builder
            thunk <- recover $ fmap join $ withPath pathItem do
              whenJust eAlias pushAliasAtCurrentPath
              result  <- tolerate $ translateExpr aeExpression
              whenJust eAlias popAndCheckAlias
              pure result
            let
              field = I.Field
                { fieldAlias      = eAlias
                , fieldValue      = thunk
                , fieldOptional   = opt
                , fieldAttributes = translateAttributes fieldAttributes
                }
              addField = biStringFields %~ (:|> (nameThunk, field))
              -- warning: we treat the alias as if it were an inlined copy of the
              -- field, instead of making an absolute reference to it
              addAlias = maybe id (\a -> biAliases %~ M.insert a (pathItem, thunk)) fAlias
            pure $ block & addField & addAlias
        pure (fields, newAliases, newBuilder)

      -- Identifier field
      LabelIdentifier i opt -> do
        fieldName <- translateIdentifier i
        let AliasedExpression {..} = fieldExpression
        -- check for conflicts from the field's name
        when (fieldName `M.member` _visibleAliases scope) $
          refute $ error "scope conflict between alias and field" -- FIXME
        when (fieldName `M.member` aliases) $
          refute $ error "scope conflict between alias and field" -- FIXME
        -- check for conflicts from the field's alias (if any)
        fAlias <- for labelAlias \a -> do
          l <- translateIdentifier a
          when (l == fieldName) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` _visibleFields scope) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `S.member` fields) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` aliases) $
            refute $ error "alias defined more than once" -- FIXME
          pure l
        -- check for conflicts from the expression's alias (if any)
        eAlias <- for aeAlias \a -> do
          l <- translateIdentifier a
          when (l == fieldName) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` _visibleFields scope) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `S.member` fields) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` aliases) $
            refute $ error "alias defined more than once" -- FIXME
          pure l
        -- finally evaluate the expression
        let
          -- WARNING: BUG?
          -- the reference claims that the alias to an optional field is only visible
          -- within the definition of that field, but the playground disagrees; we make
          -- the alias visible to the whole block to be consistent
          pathItem   = PathField fieldName
          aliasPath  = _currentPath scope :|> pathItem
          newFields  = S.insert fieldName fields
          newAliases = aliases & maybe id (flip M.insert aliasPath) fAlias
          newBuilder = do
            block <- builder
            cpath <- use currentPath
            thunk <- recover $ fmap join $ withPath pathItem do
              whenJust eAlias pushAliasAtCurrentPath
              result  <- tolerate $ translateExpr aeExpression
              whenJust eAlias popAndCheckAlias
              pure result
            let
              field = I.Field
                { fieldAlias      = eAlias
                , fieldValue      = thunk
                , fieldOptional   = opt
                , fieldAttributes = translateAttributes fieldAttributes
                }
              aThunk = Ref $ cpath :|> PathField fieldName
              addField = biIdentFields %~ M.insertWith (flip (<>)) fieldName (pure field)
              addAlias = maybe id (\a -> biAliases %~ M.insert a (pathItem, aThunk)) fAlias
            pure $ block & addField & addAlias
        pure (newFields, newAliases, newBuilder)

      -- Constraint field
      LabelConstraint con -> do
        -- WARNING: DIVERGENT BEHAVIOUR?
        -- an alias on a constraint field make no sense and can't be used in any
        -- meaningful way, so they are just entirely ignored for now
        let
          AliasedExpression consAlias consExpr = con
          AliasedExpression exprAlias exprExpr = fieldExpression

        -- check for conflicts from the conditions's alias (if any)
        cAlias <- for consAlias \a -> do
          l <- translateIdentifier a
          when (l `M.member` _visibleFields scope) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `S.member` fields) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` aliases) $
            refute $ error "alias defined more than once" -- FIXME
          pure l
        -- check for conflicts from the expression's alias (if any)
        eAlias <- for exprAlias \a -> do
          l <- translateIdentifier a
          when (l `M.member` _visibleFields scope) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `S.member` fields) $
            refute $ error "scope conflict between alias and field" -- FIXME
          when (l `M.member` aliases) $
            refute $ error "alias defined more than once" -- FIXME
          pure l
        let
          newBuilder = do
            block <- builder
            -- evaluate the condition
            condThunk <- recover $ withPath PathConstraint $ translateExpr consExpr
            -- evaluate the expression
            exprThunk <- recover do
              whenJust cAlias pushAliasAtCurrentPath
              whenJust eAlias pushAliasAtCurrentPath
              result  <- tolerate $ translateExpr exprExpr
              whenJust eAlias popAndCheckAlias
              whenJust cAlias popAndCheckAlias
              pure result
            -- TODO: this isn't enough representation, it's missing attributes and
            -- aliases
            pure $ block & biConstraints %~ (:|> (condThunk, exprThunk))
        pure (fields, aliases, newBuilder)

-- | Recursively translates a comprehension clause.
--
-- Each identifier in a clause is treated is a field, and we therefore
-- 'tolerate' that arise from processing the rest of the comprehension to make
-- sure that we pop the fields back.
translateComprehension
  :: NonEmpty ComprehensionClause
  -> [Declaration]
  -> Translation I.Embedding
translateComprehension (clause :| rest) decl =
  fmap (fromMaybe (InlineThunk $ Leaf $ Null) . join) $
    tolerate $ case clause of
      ComprehensionFor n e -> do
        l <- translateIdentifier n
        t <- translateExpr e
        recur [l] $ For l t
      ComprehensionIndexedFor i n e -> do
        j <- translateIdentifier i
        l <- translateIdentifier n
        t <- translateExpr e
        recur [j,l] $ IndexedFor j l t
      ComprehensionIf e -> do
        t <- translateExpr e
        recur [] $ If t
      ComprehensionLet n e -> do
        l <- translateIdentifier n
        t <- translateExpr e
        recur [l] $ Let l t
  where
    recur fs c = do
      -- the playground treats comprehension identifiers as fields, not aliases
      -- we do the same
      traverse_ pushField fs
      result <- tolerate $ case nonEmpty rest of
        Just cs ->
          translateComprehension cs decl <&> \case
            -- something went wrong, ignore this result
            InlineThunk t -> InlineThunk t
            -- add this clause on the way back
            I.Comprehension rcs b -> I.Comprehension (NE.cons c rcs) b
        Nothing -> do
          b <- translateBlock decl
          pure $ I.Comprehension (pure c) b
      traverse_ popField (L.reverse fs)
      pure result

translateExpr :: Expression -> Translation Thunk
translateExpr = \case
  A.Multiplication e1 e2 -> I.Multiplication <$> translateExpr e1 <*> translateExpr e2
  A.Division       e1 e2 -> I.Division       <$> translateExpr e1 <*> translateExpr e2
  A.Addition       e1 e2 -> I.Addition       <$> translateExpr e1 <*> translateExpr e2
  A.Subtraction    e1 e2 -> I.Subtraction    <$> translateExpr e1 <*> translateExpr e2
  A.Equal          e1 e2 -> I.Equal          <$> translateExpr e1 <*> translateExpr e2
  A.NotEqual       e1 e2 -> I.NotEqual       <$> translateExpr e1 <*> translateExpr e2
  A.Match          e1 e2 -> I.Match          <$> translateExpr e1 <*> translateExpr e2
  A.NotMatch       e1 e2 -> I.NotMatch       <$> translateExpr e1 <*> translateExpr e2
  A.LessThan       e1 e2 -> I.LessThan       <$> translateExpr e1 <*> translateExpr e2
  A.LessOrEqual    e1 e2 -> I.LessOrEqual    <$> translateExpr e1 <*> translateExpr e2
  A.GreaterThan    e1 e2 -> I.GreaterThan    <$> translateExpr e1 <*> translateExpr e2
  A.GreaterOrEqual e1 e2 -> I.GreaterOrEqual <$> translateExpr e1 <*> translateExpr e2
  A.LogicalAnd     e1 e2 -> I.LogicalAnd     <$> translateExpr e1 <*> translateExpr e2
  A.LogicalOr      e1 e2 -> I.LogicalOr      <$> translateExpr e1 <*> translateExpr e2
  u@A.Unification {}     -> I.Unification    <$> getU u
  d@A.Disjunction {}     -> I.Disjunction    <$> getD d
  Unary ue -> snd <$> translateUnaryExpr False ue
  where
    getU = \case
      A.Unification e1 e2 -> liftA2 (<>) (getU e1) (getU e2)
      expr                -> pure <$> translateExpr expr
    getD = \case
      A.Disjunction e1 e2 -> liftA2 (<>) (getD e1) (getD e2)
      A.Unary ue          -> pure <$> translateUnaryExpr True ue
      expr                -> pure . (False,) <$> translateExpr expr

translateUnaryExpr :: Bool -> UnaryExpression -> Translation (Bool, Thunk)
translateUnaryExpr defaultAllowed UnaryExpression {..} = do
  let (stars, ops) = L.span (== OperatorMul) ueOperators
  unless (defaultAllowed || L.null stars) $
    refute $ error "preference mark not allowed at this position" -- FIXME
  primary <- translatePrimaryExpr uePrimaryExpression
  result <- foldrM addOp primary ops
  pure (not $ L.null stars, result)
  where
    addOp = \case
      OperatorAdd      -> pure . NumId
      OperatorSub      -> pure . Negate
      OperatorNot      -> pure . LogicalNot
      OperatorNotEqual -> pure . IsNotEqualTo
      OperatorMatch    -> pure . Matches
      OperatorNotMatch -> pure . Doesn'tMatch
      OperatorLT       -> pure . IsLessThan
      OperatorLTE      -> pure . IsLessOrEqualTo
      OperatorGT       -> pure . IsGreaterThan
      OperatorGTE      -> pure . IsGreaterOrEqualTo
      OperatorMul      -> const $ refute $ error "preference mark not allowed at this position" -- FIXME
      _                -> unreachable

translatePrimaryExpr :: PrimaryExpression -> Translation Thunk
translatePrimaryExpr = \case
  PrimarySelector pe s -> do
    t <- translatePrimaryExpr pe
    l <- case s of
      Left  i -> translateIdentifier i
      Right n -> pure $ FieldLabel n Regular
    -- if we detect that the LHS was a reference, we alter the reference to
    -- point to the inner field, which avoids having to evaluate the entire
    -- reference
    pure $ case t of
      Ref p -> Ref $ p :|> PathField l
      _     -> Select t l
  PrimaryIndex pe e -> Index
    <$> translatePrimaryExpr pe
    <*> translateExpr e
  PrimarySlice pe (e1, e2) -> Slice
    <$> translatePrimaryExpr pe
    <*> translateExpr e1
    <*> translateExpr e2
  PrimaryCall pe es -> Call
    <$> translatePrimaryExpr pe
    <*> traverse translateExpr es
  PrimaryOperand o ->
    translateOperand o

translateOperand :: Operand -> Translation Thunk
translateOperand = \case
  OperandExpression e -> translateExpr e
  OperandLiteral l -> translateLiteral l
  OperandName i -> resolve i

translateLiteral :: Literal -> Translation Thunk
translateLiteral = \case
  -- recur
  A.StructLiteral s -> Block <$> translateBlock s
  A.ListLiteral   l -> translateListLiteral l
  StringLiteral i l -> translateStringLiteral i l
  -- leaves
  IntegerLiteral i  -> pure $ Leaf $ Integer i
  FloatLiteral   f  -> pure $ Leaf $ Float   f
  BoolLiteral    b  -> pure $ Leaf $ Boolean b
  NullLiteral       -> pure $ Leaf Null
  BottomLiteral     -> pure Bottom

translateListLiteral :: A.ListLiteral -> Translation Thunk
translateListLiteral = fmap List . \case
  ClosedList es    -> ListInfo <$> traverse go es <*> pure Nothing
  OpenList   es el -> ListInfo <$> traverse go es <*> case el of
    Nothing -> pure $ Just Top
    Just e  -> Just <$> translateExpr e
  where
    go = \case
      EmbeddedExpression AliasedExpression {..} -> do
        whenJust aeAlias $ const $
          refute $ error "alias in embed" -- FIXME
        thunk <- recover $ tolerate $ translateExpr aeExpression
        pure $ InlineThunk thunk
      EmbeddedComprehension A.Comprehension {..} ->
        translateComprehension compClauses compResult

translateStringLiteral :: TextInfo -> StringLiteral -> Translation Thunk
translateStringLiteral i l = do
  r <- for l \case
    A.Interpolation  e -> translateExpr e
    RawStringLiteral s -> pure $ case tiType i of
      SingleLineString -> Leaf $ String s
      MultiLinesString -> Leaf $ String s
      SingleLineBytes  -> Leaf $ Bytes  s
      MultiLinesBytes  -> Leaf $ Bytes  s
  pure $ case r of
    [Leaf (String s)] -> Leaf (String s)
    [Leaf (Bytes  s)] -> Leaf (Bytes  s)
    elems             -> I.Interpolation i elems

translateIdentifier :: Identifier -> Translation FieldLabel
translateIdentifier (Identifier i) = do
  t <- case T.unpack $ T.take 2 i of
    ['_', '_'] -> refute $ error "use of reserved identifier" -- FIXME
    ['_', '#'] -> pure HiddenDefinition
    ['_']      -> refute $ error "_ cannot be used as label" -- FIXME
    ('_':_)    -> pure Hidden
    ('#':_)    -> pure Definition
    (_:_)      -> pure Regular
    _          -> unreachable
  pure $ FieldLabel i t

translateAttributes :: [Attribute] -> Attributes
translateAttributes = foldr step mempty
  where
    step Attribute {..} =
      M.insertWith (<>) (getIdentifier attributeName) (pure attributeText)


--------------------------------------------------------------------------------
-- * Helpers

-- | If evaluation of a thunk failed, and we won't emit a translation, just
-- use @null@ instead of the missing thunk.
recover :: Functor f => f (Maybe Thunk) -> f Thunk
recover = fmap $ fromMaybe $ Leaf Null

processImport :: HashMap Text Package -> Packages -> Import -> Either Errors Packages
processImport pkgs m Import {..} = do
  (packagePath, packageName) <- translateImport importPath
  let name = maybe packageName getIdentifier importName
  Package {..} <- M.lookup packagePath pkgs
    `onNothing` Left (error "package not found") -- FIXME
  when (name `M.member` m) $
    -- the playground does not reject this?
    Left (error "duplicate package name") -- FIXME
  pure $ M.insert name pkgDocument m
  where
    illegal = "!\"#$%&'()*,:;<=>?[]^{|}\xFFFD" :: String
    invalid c = not (isPrint c) || isSpace c || elem c illegal
    validate t = when (T.null t || T.any invalid t) $
      Left (error "invalid import")
    translateImport t = do
      let (part1, part2) = T.breakOnEnd ":" t
      if T.null part1
      then do
        validate part2
        pure (part2, snd $ T.breakOnEnd "/" part2)
      else do
        validate part1
        validate part2
        pure (part1, part2)
