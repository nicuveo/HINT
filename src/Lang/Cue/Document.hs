{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Document where

import "this" Prelude

import Control.Lens          hiding (List)

import Lang.Cue.Internal.HKD
import Lang.Cue.IR           qualified as I


--------------------------------------------------------------------------------
-- * Document

-- | Representation of an interpreted cue document. It is similar to a JSON
-- document, except that any part of it might be left unevaluated, and each
-- struct or field can be annotated.
--
-- Compiling / validating the AST results in an evaluated document (see 'Thunk')
-- evaluating the document reduces it as much as possible, down to a concrete
-- value if possible, but leaving all unresolved elements as thunks.
--
-- Bottom is not represented as part of the document, since bottom represents
-- (recoverable) evaluation errors.
data Document' f
  -- structure
  = Atom         I.Atom
  | List         [HKD f (Document' f)]
  | Struct       (StructInfo' f)
  -- constraints
  | NotNull
  | BoolBound    BoolBound
  | IntegerBound IntegerBound
  | FloatBound   FloatBound
  | StringBound  StringBound
  | BytesBound   BytesBound
  -- unevaluated
  | Thunk        I.Thunk

type Document = Document' Identity

deriving instance Show (HKD f (Document' f)) => Show (Document' f)
deriving instance Eq   (HKD f (Document' f)) => Eq   (Document' f)

instance FFunctor Document' where
  ffmap :: forall f g. (HKDF f, HKDF g) => FFunction f g -> Document' f -> Document' g
  ffmap f = \case
    NotNull        -> NotNull
    Atom         x -> Atom         x
    BoolBound    x -> BoolBound    x
    IntegerBound x -> IntegerBound x
    FloatBound   x -> FloatBound   x
    StringBound  x -> StringBound  x
    BytesBound   x -> BytesBound   x
    Thunk        x -> Thunk        x
    List         l -> List   $ map (ffrecur @Document' f) l
    Struct       s -> Struct $ ffmap f s

--------------------------------------------------------------------------------
-- * Bounds

-- | Bounds represent conditions on values. They unify with a value by returning
-- the value if it matches the condition, by throwing an error otherwise. This
-- type is generic and contains the union of all possible type-specific tests
-- for a given type. Specific types can deactivate some of the tests by using
-- 'Void'.
--
-- Parameters:
--   * @e@ is the type of values in the Equality checks
--   * @o@ is the type of values in the bounds (Ordering)
--   * @r@ is the type of values in the Regex clauses
data Bound e o r = Bound
  { -- | lower bound (if any)
    minBound    :: EndPoint o
  , -- | upper bound (if any)
    maxBound    :: EndPoint o
  , -- | excluded values
    different   :: [e]
  , -- | regex match
    matchesAll  :: [r]
  , -- | regex non-match
    matchesNone :: [r]
  }
  deriving (Show, Eq)

type BoolBound    = Bound Bool    Void    Void
type IntegerBound = Bound Integer Integer Void
type FloatBound   = Bound Float   Float   Void
type StringBound  = Bound Text    Text    Text
type BytesBound   = Bound Text    Text    Void

-- | Bound for an ordered value.
data EndPoint o
  = Open
  | Inclusive o
  | Exclusive o
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Struct

data StructInfo' f = StructInfo
  { structFields     :: HashMap Text (Field' f)
  , structAttributes :: I.Attributes
  }

type StructInfo = StructInfo' Identity

deriving instance Show (HKD f (Document' f)) => Show (StructInfo' f)
deriving instance Eq   (HKD f (Document' f)) => Eq   (StructInfo' f)

instance FFunctor StructInfo' where
  ffmap f StructInfo {..} =
    StructInfo (fmap (ffmap f) structFields) structAttributes


data Field' f = Field
  { fieldValue      :: HKD f (Document' f)
  , fieldAttributes :: I.Attributes
  }

type Field = Field' Identity

deriving instance Show (HKD f (Document' f)) => Show (Field' f)
deriving instance Eq   (HKD f (Document' f)) => Eq   (Field' f)

instance FFunctor Field' where
  ffmap f Field {..} =
    Field (ffrecur @Document' f fieldValue) fieldAttributes


--------------------------------------------------------------------------------
-- * Lenses

makePrisms ''Document'

instance HasDocs (HKD f (Document' f)) f => Plated (Document' f) where
  plate f = \case
    NotNull        -> pure NotNull
    Atom         x -> pure $ Atom         x
    BoolBound    x -> pure $ BoolBound    x
    IntegerBound x -> pure $ IntegerBound x
    FloatBound   x -> pure $ FloatBound   x
    StringBound  x -> pure $ StringBound  x
    BytesBound   x -> pure $ BytesBound   x
    Thunk        x -> pure $ Thunk        x
    List         l -> List   <$> documents f l
    Struct       s -> Struct <$> documents f s

class HasDocs a f | a -> f where
  documents :: Traversal' a (Document' f)
  default documents :: (a ~ t x, Traversable t, HasDocs x f) => Traversal' a (Document' f)
  documents = traverse . documents

instance HasDocs (Document' f) f where
  documents = id

instance HasDocs a f => HasDocs [a] f

instance HasDocs a f => HasDocs (HashMap k a) f where

instance HasDocs (HKD f (Document' f)) f => HasDocs (StructInfo' f) f where
  documents f (StructInfo fs as) = StructInfo
    <$> documents f fs
    <*> pure as

instance HasDocs (HKD f (Document' f)) f => HasDocs (Field' f) f where
  documents f (Field fv as) = Field
    <$> documents f fv
    <*> pure as
