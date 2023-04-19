module Lang.Cue.Eval where

import "this" Prelude      hiding (negate, product, sum)

import Control.Lens        hiding (Empty)
import Data.HashMap.Strict qualified as M
import Data.Sequence       as S

import Lang.Cue.Error
import Lang.Cue.IR


--------------------------------------------------------------------------------
-- * Evaluation monad

{-
data Context s = Context
  { _currentPath    :: Path
  , _visibleFields  :: HashMap FieldLabel (NonEmpty Path)
    _visibleAliases :: HashMap FieldLabel (NonEmpty Bool)
  }

makeLenses ''Context


newtype Eval a = Eval { runEval :: a }
  deriving (Functor, Applicative, Monad) via Identity

eval :: Expression -> Value
eval = runEval . evalExpression
-}

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
substitutePaths old new = transform $ over _Ref \path -> sub path (old, path)
  where
    sub p = \case
      -- the old path is a prefix: substitute
      (Empty, suffix) -> new <> suffix
      -- the old path wasn't: do not substitute
      (_, Empty ) -> p
      -- compare pair-wise
      (f1 :<| fs1, f2 :<| fs2)
        | f1 == f2  -> sub p (fs1, fs2)
        | otherwise -> p


--------------------------------------------------------------------------------
-- * Alias inlining

-- | Alias can never be overloaded, and an alias and a field with the same name
-- cannot co-exist. Consequently, every occurence of a reference to an alias can
-- be replaced by the thunk associated with the alias.
--
-- Aliases are a bit more strict wrt. cycles than fields are: if an alias
-- appears within its own definition, inlining fails, even if the definitions
-- would be valid recursive fields.
--
-- For instance, this is valid:
--
--     a: b
--     b: {c:0, r: a.c}
--
-- but this isn't:
--
--     let a = b
--     let b = {c:0, r: a.c}
--
-- Furthermore, this is a "compilation error", not a bottom: an inlining error
-- makes the entire computation fail. However, this function doesn't detect
-- field cycles. But any attempt at evaluting an alias after inline is done can
-- be correctly identified as a field cycle.
--
-- Consider this contrived example:
--
--     a: z = {
--       b: {x: 0}
--       c: {a: z.b.x}
--     }
--     let f = g
--     let g = f
--     b: f
--
-- after inlining z, we get:
--
--     a: {
--       b: {x: 0}
--       c: {a: {b: {x: 0}, c: {a: z.b.x}}.b.x}
--     }
--
-- this is valid, as fields are lazy: since we only extract the @b@ field out of
-- the inlined @z@ expression, this will not result in an evaluation error
--
-- after inlining f, we get:
--
--     let f = g
--     let g = g
--     b: g
--
-- when inlining g, we detect that g contains itself, and we fail.
--
-- This function traverses the thunk hierarchy; and for each alias in each block
-- replaces all downstrean uses of that alias. WARNING: this is probably
-- exponential? can we do better by maintaining a context manually?
inlineAliases :: Thunk -> Either Error Thunk
inlineAliases = transformM $ _Block \(b@BlockInfo {..}) -> do
  let
    updatedBlock = b
      & biIdentFields  . traverse . traverse %~ inlineFieldAlias _biAbsolutePath
      & biStringFields . traverse . traverse %~ inlineFieldAlias _biAbsolutePath
  foldM inlineBlockAlias updatedBlock $ M.keys _biAliases

-- | Inline a field alias, if any.
inlineFieldAlias :: Path -> Field -> Field
inlineFieldAlias path f = case fieldAlias f of
  Nothing   -> f
  Just name -> f & thunks %~ inlineInThunk path name (fieldValue f)

-- | Given an alias appearing in a block, repla
inlineBlockAlias :: BlockInfo -> FieldLabel -> Either Error BlockInfo
inlineBlockAlias b@BlockInfo {..} alias = do
  let
    thunk = _biAliases M.! alias
    inline = inlineInThunk _biAbsolutePath alias thunk
    detect = errorOnCycle  _biAbsolutePath alias
  b
    & biIdentFields  . thunks %~ inline
    & biStringFields . thunks %~ inline
    & biEmbeddings   . thunks %~ inline
    & biConstraints  . thunks %~ inline
    & (biAliases . M.traverseWithKey) \name ->
       if name == alias then detect else pure . inline

inlineInThunk :: Path -> FieldLabel -> Thunk -> Thunk -> Thunk
inlineInThunk path name new = transform \case
  Alias p n | path == p, name == n -> new
  t -> t

errorOnCycle :: Path -> FieldLabel -> Thunk -> Either Error Thunk
errorOnCycle path name = transformM \case
  Alias p n | path == p, name == n -> Left (error "cycle!!!")
  t -> Right t
