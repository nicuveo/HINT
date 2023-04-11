module Lang.Cue.Location where

import "this" Prelude


--------------------------------------------------------------------------------
-- * Offset

-- | A position within a file, measured in characters.
newtype Offset = Offset Int
  deriving (Show)

-- | Functor that wraps a value with an offset.
newtype WithOffset a = WithOffset (Offset, a)
  deriving (Show, Functor, Foldable, Traversable)

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
  , locCode     :: [Text]
  , locOffset   :: Int
  }

-- | Show instance that hides the full content of the file.
instance Show Location where
  show (Location {..}) = "Location " <> show locFilename <> " " <> show locOffset

newtype WithLocation a = WithLocation (Location, a)
  deriving (Show, Functor, Foldable, Traversable)

instance HasOffset Location where
  getOffset = locOffset

instance HasOffset (WithLocation a) where
  getOffset (WithLocation (l, _)) = getOffset l

getLocation :: WithLocation a -> Location
getLocation (WithLocation (l, _)) = l

withLocation :: Location -> a -> WithLocation a
withLocation = WithLocation ... (,)

discardLocation :: WithLocation a -> a
discardLocation (WithLocation (_, x)) = x
