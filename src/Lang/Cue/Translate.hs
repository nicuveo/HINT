module Lang.Cue.Translate
  ( translateFile
  , translateExpression
  ) where

import "this" Prelude

import Control.Monad.Validate
import Data.HashMap.Strict    qualified as M
import Data.Text              qualified as T

import Lang.Cue.AST           as A
import Lang.Cue.Document      as D
import Lang.Cue.Error
import Lang.Cue.Package
import Lang.Cue.Tokens


--------------------------------------------------------------------------------
-- * API

translateFile
  :: HashMap Text Package
  -> SourceFile
  -> Either Errors Thunk
translateFile _pkgs SourceFile {..} = do
  imported <- undefined -- resolve import statements
  b <- runTranslation imported $ translateBlock moduleDeclarations
  pure $ Block b { defCanBeAtom = True }

translateExpression
  :: Expression
  -> Either Errors Thunk
translateExpression = runTranslation mempty . translateExpr


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
  , _visibleFields  :: HashMap Text (NonEmpty (Seq FieldLabel))
  , -- | we don't perform alias inline during the translation phase, and keep
    -- aliases in the output; furthermore, aliases can't be referenced from
    -- outside of the hierarchy in which they are declared, so there's never a
    -- need to refer to them via absolute path. the only thing we need to keep
    -- track of is whether they've been used in their scope (it's an error not
    -- to consume them [add reference link here when they document that
    -- behaviour])
    _visibleAliases :: HashMap Text (NonEmpty Bool)
  }

runTranslation :: Packages -> Translation a -> Either Errors a
runTranslation pkgs (Translation a) = a
  & flip runReaderT pkgs
  & flip evalStateT (Scope mempty mempty mempty)
  & runValidate


--------------------------------------------------------------------------------
-- * Translation rules

translateBlock :: [Declaration] -> Translation Block
translateBlock = foldM translateDeclaration $ Definitions
  { defFields      = M.empty
  , defConstraints = []
  , defAliases     = M.empty
  , defAttributes  = M.empty
  , defEmbeddings  = []
  , defCanBeAtom   = False
  }

translateDeclaration :: Block -> Declaration -> Translation Block
translateDeclaration block = \case
  DeclarationAttribute Attribute {..} ->
    pure block
      { defAttributes = M.insertWith (++) (getIdentifier attributeName) [attributeText] $ defAttributes block
      }
  DeclarationLetClause LetClause {..} -> do
    fieldLabel <- translateIdentifier letName
    when (fieldLabel `M.member` defFields block) $
      refute $ error "scope conflict between alias and field" -- FIXME
    when (fieldLabel `M.member` defAliases block) $
      refute $ error "alias defined more than once" -- FIXME
    pure block
  _ -> undefined

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
