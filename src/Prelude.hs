-- | Internal module that re-exports the regular prelude, plus some
-- common useful functions.

module Prelude
  ( -- * re-exports from useful "default" modules
    module P
    -- * custom operators
  , (...)
  ) where


--------------------------------------------------------------------------------
-- Re-exports

import Control.Applicative    as P (liftA2)
import Control.Arrow          as P (first, left, second, (&&&), (***), (<<<),
                                    (>>>))
import Control.Monad          as P (unless, void, when)
import Control.Monad.Identity as P (Identity (..))
import Data.Bool              as P (bool)
import Data.Either            as P (lefts, partitionEithers, rights)
import Data.Foldable          as P (asum, fold, foldMap', foldlM, foldrM, for_,
                                    toList, traverse_)
import Data.Function          as P (on, (&))
import Data.Functor           as P (($>), (<&>))
import Data.HashMap.Strict    as P (HashMap, mapKeys)
import Data.HashSet           as P (HashSet)
import Data.List              as P (find, findIndex, foldl', group, intercalate,
                                    intersect, lookup, sort, sortBy, sortOn,
                                    union, unionBy, (\\))
import Data.List.NonEmpty     as P (NonEmpty (..), nonEmpty)
import Data.Maybe             as P (catMaybes, fromMaybe, isJust, isNothing,
                                    listToMaybe, maybeToList)
import Data.Ord               as P (comparing)
import Data.Semigroup         as P (Semigroup (..))
import Data.Sequence          as P (Seq)
import Data.String            as P (IsString)
import Data.Text              as P (Text)
import Data.Traversable       as P (for)
import Data.Void              as P (Void, absurd)
import GHC.Generics           as P (Generic)
import "base" Prelude         as P hiding (lookup)


--------------------------------------------------------------------------------
-- Operators

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f ... g = \x y -> f (g x y)
infixr 8 ...
