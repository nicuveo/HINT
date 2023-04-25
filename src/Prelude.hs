{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Internal module that re-exports the regular prelude, plus some
-- common useful functions.

module Prelude
  ( -- * re-exports from useful "default" modules
    module P
    -- * custom operators
  , (...)
    -- * list helpers
  , paddedZip
  , paddedZipWith
    -- * maybe helpers
  , onNothing
  , onNothingM
    -- * either helpers
  , onLeft
  , onLeftM
    -- * sequence helpers
  , Seq (.., Lone)
  , nubBy
    -- * hashmap helpers
  , unionWithM
    -- * nested fmaps
  , fmap2
  , (<<$>>)
  , (<<&>>)
    -- * lens
  , noopLens
  ) where


--------------------------------------------------------------------------------
-- Re-exports

import Control.Applicative             as P (liftA, liftA2)
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

import Control.Lens                    hiding (Empty, (...))
import Control.Monad.Validate
import Control.Monad.Validate.Internal
import Data.HashMap.Strict             qualified as M
import Data.Sequence                   (Seq (..))


--------------------------------------------------------------------------------
-- Operators

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f ... g = \x y -> f (g x y)
infixr 8 ...


--------------------------------------------------------------------------------
-- List helpers

paddedZip :: a -> b -> [a] -> [b] -> [(a, b)]
paddedZip = paddedZipWith (,)

paddedZipWith :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
paddedZipWith _ _ _    []     []  = []
paddedZipWith f a _    []     bs  = zipWith f (repeat a) bs
paddedZipWith f _ b    as     []  = zipWith f as (repeat b)
paddedZipWith f a b (x:as) (y:bs) = f x y : paddedZipWith f a b as bs


--------------------------------------------------------------------------------
-- Maybe helpers

onNothing :: Applicative m => Maybe a -> m a -> m a
onNothing a d = maybe d pure a

onNothingM :: Monad m => m (Maybe a) -> m a -> m a
onNothingM a d = a >>= flip onNothing d


--------------------------------------------------------------------------------
-- Either helpers

onLeft :: Applicative m => Either e a -> (e -> m a) -> m a
onLeft a f = either f pure a

onLeftM :: Monad m => m (Either e a) -> (e -> m a) -> m a
onLeftM a f = a >>= flip onLeft f


--------------------------------------------------------------------------------
-- Sequence helpers

pattern Lone :: a -> Seq a
pattern Lone x = x :<| Empty

nubBy :: (a -> a -> Bool) -> Seq a -> Seq a
nubBy eq = foldl' go Empty
  where
    go res x
      | any (eq x) res = res
      | otherwise      = res :|> x


--------------------------------------------------------------------------------
-- HashMap helpers

unionWithM ::
  (Monad m, Hashable k) =>
  (v -> v -> m v) ->
  HashMap k v ->
  HashMap k v ->
  m (HashMap k v)
unionWithM f m1 m2 = foldM step m1 (M.toList m2)
  where
    step m (k, new) = case M.lookup k m of
      Nothing -> pure $ M.insert k new m
      Just old -> do
        combined <- f new old
        pure $ M.insert k combined m


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
-- Lens

noopLens :: Lens s s (Maybe a) (Maybe b)
noopLens = lens (const Nothing) const


--------------------------------------------------------------------------------
-- Missing instances from third-party libraries

instance MonadFix m => MonadFix (ValidateT e m) where
  mfix f = ValidateT $ mfix \a -> getValidateT (f a)
