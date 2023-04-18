{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains the functions that translate from the AST into an
-- unevaluated 'Thunk'.
module Lang.Cue.Translate
  ( translateFile
  , translateExpression
  ) where

import "this" Prelude

import Control.Lens           (ix, makeLenses, use, uses, (%=), (%~), (.=))
import Control.Monad.Extra    (unlessM, whenJust)
import Control.Monad.Validate
import Data.HashMap.Strict    qualified as M
import Data.List              qualified as L
import Data.List.NonEmpty     as NE
import Data.Sequence          as S
import Data.Text              qualified as T

import Lang.Cue.AST           as A
import Lang.Cue.Document      as D
import Lang.Cue.Error
import Lang.Cue.Package
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

type Packages = HashMap Text Package

-- | Stores the current scope during the translation phase. For each kind of
-- bindings and for each name, we store a stack: on the way back up, we can pop
-- the values we added and restore the previous context.
data Scope = Scope
  { _currentPath    :: Seq FieldLabel
  , _visibleFields  :: HashMap FieldLabel (NonEmpty (Seq FieldLabel))
  , -- | we don't perform alias inline during the translation phase, and keep
    -- aliases in the output; furthermore, aliases can't be referenced from
    -- outside of the hierarchy in which they are declared, so there's never a
    -- need to refer to them via absolute path. the only thing we need to keep
    -- track of is whether they've been used in their scope (it's an error not
    -- to consume them [add reference link here when they document that
    -- behaviour])
    _visibleAliases :: HashMap FieldLabel (NonEmpty Bool)
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
  -> Either Errors Thunk
translateFile _pkgs SourceFile {..} = do
  imported <- undefined -- resolve import statements
  b <- runTranslation imported $ translateBlock moduleDeclarations
  pure $ Block b { _defCanBeAtom = True }

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
withPath :: FieldLabel -> Translation a -> Translation (Maybe a)
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
  visibleFields %= M.insertWith (<>) f (pure $ parentPath :|> f)

-- | Pop the given field.
popField :: FieldLabel -> Translation ()
popField f = visibleFields %= flip M.update f \(_ :| t) -> nonEmpty t

-- | Add the given alias to the current scope, labelled as unused.
pushAlias :: FieldLabel -> Translation ()
pushAlias = pushAliasWith False

-- | Add the given alias to the current scope, with the given used status.
pushAliasWith :: Bool -> FieldLabel -> Translation ()
pushAliasWith b l = visibleAliases %= M.insertWith (<>) l (pure b)

-- | Mark the given alias as used. This function does NOT check that the alias
-- exists.
useAlias :: FieldLabel -> Translation ()
useAlias l = visibleAliases . ix l . ix 0 .= True

-- | Removes the given alias from the stack, and returns whether it was used.
popAlias :: FieldLabel -> Translation Bool
popAlias l =
  uses visibleAliases (M.lookup l) >>= \case
    -- if we reach this, it means that we tried to pop a non-existant alias
    Nothing -> unreachable
    Just (b :| r) -> do
      visibleAliases %= flip M.update l (const $ nonEmpty r)
      pure b

-- | Pushes all the definition in a block onto the current scope, runs the given
-- action, then pops the newly added fields. This function will raise non-fatal
-- errors if it detects that an alias was popped unused (see 'Scope').
-- Like 'withPath', it 'tolerates' errors from the given action, to ensure we
-- end up with a correct scope.
withScope :: Block -> Translation a -> Translation (Maybe a)
withScope block action = do
  let aliases = M.keys $ _defAliases block
      fields  = M.keys $ _defFields  block
  traverse_ pushAlias aliases
  traverse_ pushField fields
  result <- tolerate action
  traverse_ popField fields
  for_ aliases \alias ->
    unlessM (popAlias alias) $
      dispute $ error "alias not used!" -- FIXME
  pure result


--------------------------------------------------------------------------------
-- * Translation rules

translateBlock :: [Declaration] -> Translation Block
translateBlock decls = mdo
  let startBlock = Definitions
        { _defAttributes  = M.empty
        , _defAliases     = M.empty
        , _defEmbeddings  = S.empty
        , _defFields      = M.empty
        , _defConstraints = S.empty
        , _defCanBeAtom   = False
        }
  -- We rely on 'MonadFix' to deal with all definitions in one traversal. We
  -- need to know about the names of all of the new fields and aliases to be
  -- able to update the current scope to translate them recursively, and it's
  -- easier to do this in one traversal rather than two.
  block <-
    fmap (fromMaybe startBlock) $
      withScope block $
        foldM translateDeclaration startBlock decls
  pure block

translateDeclaration :: Block -> Declaration -> Translation Block
translateDeclaration block = \case
  DeclarationAttribute Attribute {..} ->
    pure $
      block & defAttributes %~
        M.insertWith (flip (<>)) (getIdentifier attributeName) (pure attributeText)
  DeclarationLetClause LetClause {..} -> do
    fieldLabel <- translateIdentifier letName
    when (fieldLabel `M.member` _defFields block) $
      refute $ error "scope conflict between alias and field" -- FIXME
    when (fieldLabel `M.member` _defAliases block) $
      refute $ error "alias defined more than once" -- FIXME
    thunk <- recover $ fmap join $ withPath fieldLabel do
      -- WARNING: UNDOCUMENTED BEHAVIOUR
      -- special case for let clauses: we have to remove the let identifier
      -- itself from the scope, to prevent let clause self-recursion, and to
      -- prevent shadowing previous let clauses with the same name; this is
      -- undocumented behaviour, and it's unclear whether it is on purpose or
      -- not
      wasUsed <- popAlias fieldLabel
      result  <- tolerate $ translateExpr letExpression
      pushAliasWith wasUsed fieldLabel
      pure result
    pure $
      block & defAliases %~
        M.insert fieldLabel (Right thunk)
  DeclarationEmbedding (EmbeddedExpression AliasedExpression {..}) -> do
    -- WARNING: UNDOCUMENTED BEHAVIOUR
    -- the reference claims that an embed can be an aliased expression, but the
    -- playground rejects it. in doubt, we do the same
    whenJust aeAlias $ const $
      refute $ error "alias in embed" -- FIXME
    thunk <- recover $ tolerate $ translateExpr aeExpression
    pure $ block & defEmbeddings %~ (:|> InlineThunk thunk)
  DeclarationEmbedding (EmbeddedComprehension A.Comprehension {..}) -> do
    embed <- translateComprehension compClauses compResult
    pure $ block & defEmbeddings %~ (:|> embed)
  DeclarationEllipsis _ ->
    -- Not supported yet, as per the reference.
    refute $ error "ellipsis in struct" -- FIXME
  DeclarationField A.Field {..} -> do
    undefined

-- | Recursively translates a comprehension clause.
--
-- Each identifier in a clause is treated is a field, and we therefore
-- 'tolerate' that arise from processing the rest of the comprehension to make
-- sure that we pop the fields back.
translateComprehension
  :: NonEmpty ComprehensionClause
  -> [Declaration]
  -> Translation D.Embedding
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
            D.Comprehension rcs b -> D.Comprehension (NE.cons c rcs) b
        Nothing -> do
          b <- translateBlock decl
          pure $ D.Comprehension (pure c) b
      traverse_ popField (L.reverse fs)
      pure result


translateIdentifier :: Identifier -> Translation FieldLabel
translateIdentifier (Identifier i) = do
  t <- case T.unpack $ T.take 2 i of
    ['_', '_'] -> refute undefined -- FIXME: use of reserved identifier
    ['_', '#'] -> pure HiddenDefinition
    ('_':_)    -> pure Hidden
    ('#':_)    -> pure Definition
    (_:_)      -> pure Regular
    _          -> refute undefined -- FIXME: empty identifier
  pure $ FieldLabel i t

translateExpr :: Expression -> Translation Thunk
translateExpr = undefined


--------------------------------------------------------------------------------
-- * Helpers

-- | If evaluation of a thunk failed, and we won't emit a translation, just
-- use @null@ instead of the missing thunk.
recover :: Functor f => f (Maybe Thunk) -> f Thunk
recover = fmap $ fromMaybe $ Leaf Null
