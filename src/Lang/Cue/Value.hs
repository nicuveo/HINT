{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Value where

import "this" Prelude

import Control.Lens          hiding (List)
import Data.Scientific

import Lang.Cue.AST          qualified as A
import Lang.Cue.Internal.HKD
import Lang.Cue.IR           qualified as I


--------------------------------------------------------------------------------
-- * Value

-- | Representation of a value, used during the evaluation.
data Value' f
  = Top
  | Atom         I.Atom
  | List         (ListInfo' f)
  | Struct       (StructInfo' f)
  | NotNull
  | Type         I.Type
  | IntegerBound IntegerBound
  | FloatBound   FloatBound
  | StringBound  StringBound
  | BytesBound   BytesBound
  | Thunk        I.Thunk
  | Disjoint     (Seq (Value' f)) (Seq (Value' f))

type Value = Value' Identity

deriving instance Show (HKD f (Value' f)) => Show (Value' f)
deriving instance Eq   (HKD f (Value' f)) => Eq   (Value' f)

instance FFunctor Value' where
  ffmap f = \case
    Top            -> Top
    NotNull        -> NotNull
    Atom         x -> Atom         x
    Type         x -> Type         x
    IntegerBound x -> IntegerBound x
    FloatBound   x -> FloatBound   x
    StringBound  x -> StringBound  x
    BytesBound   x -> BytesBound   x
    Thunk        x -> Thunk        x
    List         l -> List     (ffmap f l)
    Struct       s -> Struct   (ffmap f s)
    Disjoint   v d -> Disjoint (ffmap f <$> v) (ffmap f <$> d)


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
data Bound o r = Bound
  { -- | lower bound (if any)
    _above       :: Endpoint o
  , -- | upper bound (if any)
    _below       :: Endpoint o
  , -- | excluded values
    _different   :: [o]
  , -- | regex match
    _matchesAll  :: [r]
  , -- | regex non-match
    _matchesNone :: [r]
  }
  deriving (Show, Eq)

unbound :: Bound o r
unbound = Bound Open Open [] [] []

type IntegerBound = Bound Integer    Void
type FloatBound   = Bound Scientific Void
type StringBound  = Bound Text       Text
type BytesBound   = Bound Text       Void

-- | Bound for an ordered value.
data Endpoint o
  = Open
  | Inclusive o
  | Exclusive o
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Containers

data ListInfo' f = ListInfo
  { _lValues  :: Seq (HKD f (Value' f))
  , _lDefault :: Seq (HKD f (Value' f))
  }

type ListInfo = ListInfo' Identity

deriving instance Show (HKD f (Value' f)) => Show (ListInfo' f)
deriving instance Eq   (HKD f (Value' f)) => Eq   (ListInfo' f)

instance FFunctor ListInfo' where
  ffmap f ListInfo {..} = ListInfo
    (_lValues  & traverse %~ ffrecur @Value' f)
    (_lDefault & traverse %~ ffrecur @Value' f)


data StructInfo' f = StructInfo
  { _sFields      :: HashMap I.FieldLabel (Field' f)
  , _sConstraints :: Seq (HKD f (Value' f), HKD f (Value' f))
  -- TODO: add string fields
  , _sAttributes  :: I.Attributes
  , _sClosed      :: Bool
  }

type StructInfo = StructInfo' Identity

deriving instance Show (HKD f (Value' f)) => Show (StructInfo' f)
deriving instance Eq   (HKD f (Value' f)) => Eq   (StructInfo' f)

instance FFunctor StructInfo' where
  ffmap f StructInfo {..} = StructInfo
    (fmap (ffmap f) _sFields)
    (_sConstraints & traverse . both %~ ffrecur @Value' f)
    _sAttributes
    _sClosed


data Field' f = Field
  { _fValue      :: HKD f (Value' f)
  , _fOptional   :: A.Optional
  , _fAttributes :: I.Attributes
  }

type Field = Field' Identity

deriving instance Show (HKD f (Value' f)) => Show (Field' f)
deriving instance Eq   (HKD f (Value' f)) => Eq   (Field' f)

instance FFunctor Field' where
  ffmap f Field {..} =
    Field (ffrecur @Value' f _fValue) _fOptional _fAttributes


--------------------------------------------------------------------------------
-- * Lenses

makeLenses ''Value'
makeLenses ''Bound
makeLenses ''ListInfo'
makeLenses ''StructInfo'
makeLenses ''Field'
makePrisms ''Value'
makePrisms ''Endpoint

instance HasValue (HKD f (Value' f)) f => Plated (Value' f) where
  plate f = \case
    Top            -> pure Top
    NotNull        -> pure NotNull
    Atom         x -> pure $ Atom         x
    Type         x -> pure $ Type         x
    IntegerBound x -> pure $ IntegerBound x
    FloatBound   x -> pure $ FloatBound   x
    StringBound  x -> pure $ StringBound  x
    BytesBound   x -> pure $ BytesBound   x
    Thunk        x -> pure $ Thunk        x
    Disjoint   l s -> Disjoint <$> values f l <*> values f s
    List         l -> List     <$> values f l
    Struct       s -> Struct   <$> values f s

class HasValue a f | a -> f where
  values :: Traversal' a (Value' f)
  default values :: (a ~ t x, Traversable t, HasValue x f) => Traversal' a (Value' f)
  values = traverse . values

instance HasValue (Value' f) f where
  values = id

instance HasValue a f => HasValue (Seq a) f

instance HasValue a f => HasValue (HashMap k a) f where

instance HasValue (HKD f (Value' f)) f => HasValue (ListInfo' f) f where
  values f (ListInfo {..}) = ListInfo
    <$> values f _lValues
    <*> pure _lDefault -- ON PURPOSE, WE IGNORE CONSTRAINTS

instance HasValue (HKD f (Value' f)) f => HasValue (StructInfo' f) f where
  values f (StructInfo {..}) = StructInfo
    <$> values f _sFields
    <*> pure _sConstraints -- ON PURPOSE, WE IGNORE CONSTRAINTS
    <*> pure _sAttributes
    <*> pure _sClosed

instance HasValue (HKD f (Value' f)) f => HasValue (Field' f) f where
  values f (Field fv o as) = Field
    <$> values f fv
    <*> pure o
    <*> pure as
