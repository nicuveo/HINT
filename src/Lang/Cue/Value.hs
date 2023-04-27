{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Value where

import "this" Prelude

import Control.Lens                    hiding (List)
import Data.HashMap.Strict             qualified as M
import Data.Scientific
import Data.Sequence                   qualified as S

import Lang.Cue.AST                    qualified as A
import Lang.Cue.Internal.HKD
import Lang.Cue.Internal.IndexedPlated
import Lang.Cue.IR                     qualified as I


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
  | Thunk        I.Thunk
  | List         (ListInfo' f)
  | Struct       (StructInfo' f)
  | Disjoint     (Seq (Value' f)) (Seq (Value' f))

type Value     = Value' Identity
type WHNFValue = Value' (Const I.Thunk)

deriving instance Show (HKD f (Value' f)) => Show (Value' f)
deriving instance Eq   (HKD f (Value' f)) => Eq   (Value' f)

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

type ListInfo     = ListInfo' Identity
type WHNFListInfo = ListInfo' (Const I.Thunk)

deriving instance Show (HKD f (Value' f)) => Show (ListInfo' f)
deriving instance Eq   (HKD f (Value' f)) => Eq   (ListInfo' f)

instance FFunctor ListInfo' where
  ffmap f ListInfo {..} = ListInfo
    (_lValues  & traverse %~ ffrecur @Value' f)
    (_lDefault & traverse %~ ffrecur @Value' f)


data StructInfo' f = StructInfo
  { _sFields      :: HashMap I.FieldLabel (Field' f)
  , _sConstraints :: Seq (Value' f, HKD f (Value' f))
  -- TODO: add string fields
  , _sAttributes  :: I.Attributes
  , _sClosed      :: Bool
  }

type StructInfo     = StructInfo' Identity
type WHNFStructInfo = StructInfo' (Const I.Thunk)

deriving instance Show (HKD f (Value' f)) => Show (StructInfo' f)
deriving instance Eq   (HKD f (Value' f)) => Eq   (StructInfo' f)

instance FFunctor StructInfo' where
  ffmap f StructInfo {..} = StructInfo
    (fmap (ffmap f) _sFields)
    (_sConstraints <&> \(key, val) -> (ffmap f key, ffrecur @Value' f val))
    _sAttributes
    _sClosed


data Field' f = Field
  { _fValue      :: HKD f (Value' f)
  , _fOptional   :: A.Optional
  , _fAttributes :: I.Attributes
  }

type Field     = Field' Identity
type WHNFField = Field' (Const I.Thunk)

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
    Func         x -> pure $ Func         x
    Type         x -> pure $ Type         x
    IntegerBound x -> pure $ IntegerBound x
    FloatBound   x -> pure $ FloatBound   x
    StringBound  x -> pure $ StringBound  x
    BytesBound   x -> pure $ BytesBound   x
    Thunk        x -> pure $ Thunk        x
    Disjoint   l s -> Disjoint <$> values f l <*> values f s
    List         l -> List     <$> values f l
    Struct       s -> Struct   <$> values f s

instance HasValue (HKD f (Value' f)) f => IndexedPlated I.Path (Value' f) where
  indexedPlate path g = \case
    Top            -> pure Top
    NotNull        -> pure NotNull
    Atom         x -> pure $ Atom         x
    Func         x -> pure $ Func         x
    Type         x -> pure $ Type         x
    IntegerBound x -> pure $ IntegerBound x
    FloatBound   x -> pure $ FloatBound   x
    StringBound  x -> pure $ StringBound  x
    BytesBound   x -> pure $ BytesBound   x
    Thunk        x -> pure $ Thunk        x
    Disjoint   l s -> Disjoint <$> indexedValues path g l <*> indexedValues path g s
    List         l -> List     <$> indexedValues path g l
    Struct       s -> Struct   <$> indexedValues path g s

class HasValue a f | a -> f where
  values :: Traversal' a (Value' f)
  indexedValues :: I.Path -> IndexedTraversal' I.Path a (Value' f)

  default values :: (a ~ t x, Traversable t, HasValue x f) => Traversal' a (Value' f)
  values = traverse . values
  default indexedValues :: (a ~ t x, Traversable t, HasValue x f) => I.Path -> IndexedTraversal' I.Path a (Value' f)
  indexedValues p = traverse . indexedValues p

instance HasValue (Value' f) f where
  values = id
  indexedValues path fi = indexed fi path

instance HasValue a f => HasValue (Seq a) f

instance HasValue a f => HasValue (HashMap k a) f where

instance HasValue (HKD f (Value' f)) f => HasValue (ListInfo' f) f where
  values f (ListInfo {..}) = ListInfo
    <$> values f _lValues
    <*> pure _lDefault -- ON PURPOSE, WE IGNORE CONSTRAINTS
  indexedValues path f (ListInfo {..}) = ListInfo
    <$> S.traverseWithIndex (\i -> indexedValues (path :|> I.PathEmbedding i) f) _lValues
    <*> pure _lDefault -- ON PURPOSE, WE IGNORE CONSTRAINTS

instance HasValue (HKD f (Value' f)) f => HasValue (StructInfo' f) f where
  values f (StructInfo {..}) = StructInfo
    <$> values f _sFields
    <*> pure _sConstraints -- ON PURPOSE, WE IGNORE CONSTRAINTS
    <*> pure _sAttributes
    <*> pure _sClosed
  indexedValues path f (StructInfo {..}) = StructInfo
    <$> M.traverseWithKey (\k -> indexedValues (path :|> I.PathField k) f) _sFields
    <*> pure _sConstraints -- ON PURPOSE, WE IGNORE CONSTRAINTS
    <*> pure _sAttributes
    <*> pure _sClosed

instance HasValue (HKD f (Value' f)) f => HasValue (Field' f) f where
  values f (Field fv o as) = Field
    <$> values f fv
    <*> pure o
    <*> pure as
  indexedValues path f (Field fv o as) = Field
    <$> indexedValues path f fv
    <*> pure o
    <*> pure as
