module Lang.Cue.Location where

import "this" Prelude


--------------------------------------------------------------------------------
-- * Offset

-- | A position within a file, measured in characters.
newtype Offset = Offset Int
  deriving (Show, Eq)

-- | Functor that wraps a value with an offset.
newtype WithOffset a = WithOffset (Offset, a)
  deriving (Functor, Foldable, Traversable)

class HasOffset a where
  getOffset :: a -> Int

instance HasOffset Offset where
  getOffset (Offset x) = x

instance HasOffset (WithOffset a) where
  getOffset (WithOffset (o, _)) = getOffset o

withOffset :: Offset -> a -> WithOffset a
withOffset = WithOffset ... (,)

discardOffset :: WithOffset a -> a
discardOffset (WithOffset (_, x)) = x


--------------------------------------------------------------------------------
-- * Location

data Location = Location
  { locFilename :: String
  , locRow      :: Int
  , locColumn   :: Int
  , locCode     :: [String] -- FIXME: Text?
  }

newtype WithLocation a = WithLocation (Location, a)
  deriving (Functor, Foldable, Traversable)

withLocation :: Location -> a -> WithLocation a
withLocation = WithLocation ... (,)

discardLocation :: WithLocation a -> a
discardLocation (WithLocation (_, x)) = x
