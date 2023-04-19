module Lang.Cue.Eval where

import "this" Prelude hiding (negate, product, sum)

import Control.Lens   (over, transform)
import Data.Sequence  as S

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
-- @[#a, b]@. When substituting @#a@ in @f@, however, we must alter that path to
-- point to @f.b@; if we don't, we get something equivalent to:
--
--     f: {
--       b: 0
--       c: #a.b + 1 // whoops!
--     }
--
-- What we must do when inlining @#a@ is alter all of its inner references (any
-- reference that has an absolute path that starts with @#a@) to make it point
-- to the new path instead. What we want in the end is for @f@ to be:
--
--     f: {
--       b: 0
--       c: f.b + 1
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
