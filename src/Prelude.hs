{-# OPTIONS_GHC -Wno-orphans #-}

-- | Internal module that re-exports the regular prelude, plus some
-- common useful functions.

module Prelude
  ( -- * re-exports from useful "default" modules
    module P
    -- * custom operators
  , (...)
    -- * maybe helpers
  , onNothing
  , onNothingM
    -- * nested fmaps
  , fmap2
  , (<<$>>)
  , (<<&>>)
  ) where


--------------------------------------------------------------------------------
-- Re-exports

import Control.Applicative             as P (liftA2)
import Control.Arrow                   as P (first, left, second, (&&&), (***),
                                             (<<<), (>>>))
import Control.Monad.Except            as P
import Control.Monad.Identity          as P
import Control.Monad.Reader            as P
import Control.Monad.State.Strict      as P
import Control.Monad.Trans.Maybe       as P (MaybeT (..))
import Data.Bifunctor                  as P (bimap)
import Data.Bool                       as P (bool)
import Data.Either                     as P (lefts, partitionEithers, rights)
import Data.Foldable                   as P (asum, fold, foldMap', foldlM,
                                             foldrM, for_, toList, traverse_)
import Data.Function                   as P (on, (&))
import Data.Functor                    as P (($>), (<&>))
import Data.Hashable                   as P (Hashable)
import Data.HashMap.Strict             as P (HashMap, mapKeys)
import Data.HashSet                    as P (HashSet)
import Data.List                       as P (find, findIndex, foldl', group,
                                             intercalate, intersect,
                                             intersperse, lookup, sort, sortBy,
                                             sortOn, union, unionBy, (\\))
import Data.List.NonEmpty              as P (NonEmpty (..), nonEmpty)
import Data.Maybe                      as P (catMaybes, fromMaybe, isJust,
                                             isNothing, listToMaybe,
                                             maybeToList)
import Data.Ord                        as P (comparing)
import Data.Semigroup                  as P (Semigroup (..))
import Data.Sequence                   as P (Seq)
import Data.String                     as P (IsString)
import Data.Text                       as P (Text)
import Data.Traversable                as P (for)
import Data.Void                       as P (Void, absurd)
import GHC.Generics                    as P (Generic)
import "base" Prelude                  as P hiding (lookup)


--------------------------------------------------------------------------------
-- Internal imports

import Control.Monad.Validate
import Control.Monad.Validate.Internal


--------------------------------------------------------------------------------
-- Operators

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f ... g = \x y -> f (g x y)
infixr 8 ...


--------------------------------------------------------------------------------
-- Maybe helpers

onNothing :: Applicative m => Maybe a -> m a -> m a
onNothing a d = maybe d pure a

onNothingM :: Monad m => m (Maybe a) -> m a -> m a
onNothingM a d = a >>= flip onNothing d


--------------------------------------------------------------------------------
-- Nested fmap

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap2
infixl 4 <<$>>

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip fmap2
infixl 1 <<&>>


--------------------------------------------------------------------------------
-- Missing instances from third-party libraries

instance MonadFix m => MonadFix (ValidateT e m) where
  mfix f = ValidateT $ mfix \a -> getValidateT (f a)
