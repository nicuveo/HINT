module Lang.Cue.Internal.IndexedPlated where

import "this" Prelude

import Control.Applicative
import Control.Lens


class IndexedPlated i a where
  indexedPlate :: i -> IndexedTraversal' i a a

itransform
  :: IndexedPlated i a
  => (i -> a -> a)
  -> i
  -> a
  -> a
itransform = itransformOf indexedPlate

itransformOf
  :: (i -> IndexedTraversal' i a a)
  -> (i -> a -> a)
  -> i
  -> a
  -> a
itransformOf l f = go where
  go i = f i . iover (l i) go

itransformM
  :: (Monad m, IndexedPlated i a)
  => (i -> a -> m a)
  -> i
  -> a
  -> m a
itransformM = itransformMOf indexedPlate

itransformMOf
  :: (Monad m)
  => (i -> IndexedLensLike i (WrappedMonad m) a a a a)
  -> (i -> a -> m a)
  -> i
  -> a
  -> m a
itransformMOf l f = go where
  go i t = imapMOf (l i) go t >>= f i
