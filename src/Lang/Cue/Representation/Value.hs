{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Representation.Value where

import "this" Prelude

import Control.Lens                hiding (List)
import Data.Scientific

import Lang.Cue.Internal.HKD
import Lang.Cue.Representation.AST qualified as A
import Lang.Cue.Representation.IR  qualified as I


--------------------------------------------------------------------------------
-- * Value

-- | Representation of a value, used during the evaluation.
data Value' f
  = Top
  | Atom         I.Atom
  | Func         I.Function
  | NotNull
  | Type         I.Type
  | IntegerBound IntegerBound
  | FloatBound   FloatBound
  | StringBound  StringBound
  | BytesBound   BytesBound
  | List         (ListInfo' f)
  | Struct       (StructInfo' f)
  | Disjoint     (Seq (Value' f)) (Seq (Value' f))

type Value = Value' Identity

deriving instance Show (HKD f I.Thunk) => Show (Value' f)
deriving instance Eq   (HKD f I.Thunk) => Eq   (Value' f)

instance FFunctor Value' where
  ffmap f = \case
    Top            -> Top
    NotNull        -> NotNull
    Atom         x -> Atom         x
    Func         x -> Func         x
    Type         x -> Type         x
    IntegerBound x -> IntegerBound x
    FloatBound   x -> FloatBound   x
    StringBound  x -> StringBound  x
    BytesBound   x -> BytesBound   x
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
  { _lValues  :: Seq (HKD f I.Thunk)
  , _lDefault :: Seq (HKD f I.Thunk)
  }

type ListInfo = ListInfo' Identity

deriving instance Show (HKD f I.Thunk) => Show (ListInfo' f)
deriving instance Eq   (HKD f I.Thunk) => Eq   (ListInfo' f)

instance FFunctor ListInfo' where
  ffmap f ListInfo {..} = ListInfo
    (_lValues  & traverse %~ ffapply @I.Thunk f)
    (_lDefault & traverse %~ ffapply @I.Thunk f)


data StructInfo' f = StructInfo
  { _sFields      :: HashMap I.FieldLabel (Field' f)
  , _sConstraints :: Seq (Value' f, HKD f I.Thunk)
  -- TODO: add string fields
  , _sAttributes  :: I.Attributes
  , _sClosed      :: Bool
  }

type StructInfo = StructInfo' Identity

deriving instance Show (HKD f I.Thunk) => Show (StructInfo' f)
deriving instance Eq   (HKD f I.Thunk) => Eq   (StructInfo' f)

instance FFunctor StructInfo' where
  ffmap f StructInfo {..} = StructInfo
    (fmap (ffmap f) _sFields)
    (_sConstraints <&> \(key, val) -> (ffmap f key, ffapply @I.Thunk f val))
    _sAttributes
    _sClosed


data Field' f = Field
  { _fValue      :: HKD f I.Thunk
  , _fOptional   :: A.Optional
  , _fAttributes :: I.Attributes
  }

type Field = Field' Identity

deriving instance Show (HKD f I.Thunk) => Show (Field' f)
deriving instance Eq   (HKD f I.Thunk) => Eq   (Field' f)

instance FFunctor Field' where
  ffmap f Field {..} =
    Field (ffapply @I.Thunk f _fValue) _fOptional _fAttributes


--------------------------------------------------------------------------------
-- * Lenses

makeLenses ''Value'
makeLenses ''Bound
makeLenses ''ListInfo'
makeLenses ''StructInfo'
makeLenses ''Field'
makePrisms ''Value'
makePrisms ''Endpoint
