module Lang.Cue.Inline
  ( substitutePaths
  , inlineAliases
  ) where

import "this" Prelude

import Control.Lens                    hiding (Empty, List, below, op)
import Data.HashMap.Strict             qualified as M

import Lang.Cue.Error
import Lang.Cue.Internal.IndexedPlated
import Lang.Cue.IR                     as I


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
