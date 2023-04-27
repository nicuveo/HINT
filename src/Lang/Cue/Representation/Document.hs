{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Representation.Document where

import "this" Prelude

import Control.Lens                    hiding (List)
import Data.HashMap.Strict             qualified as M
import Data.Sequence                   qualified as S

import Lang.Cue.Internal.HKD
import Lang.Cue.Internal.IndexedPlated
import Lang.Cue.Representation.IR      qualified as I
import Lang.Cue.Representation.Value   qualified as V


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
data Document' u f
  = Atom         I.Atom
  | List         (Seq (HKD f (Document' u f)))
  | Struct       (StructInfo' u f)
  | Unresolved   (u f)

data Unresolved' f
  = Top
  | NotNull
  | Type         I.Type
  | Func         I.Function
  | IntegerBound V.IntegerBound
  | FloatBound   V.FloatBound
  | StringBound  V.StringBound
  | BytesBound   V.BytesBound
  | Disjoint     (Seq (HKD f (Document' Unresolved' f))) (Seq (HKD f (Document' Unresolved' f)))
  | Thunk        I.Thunk

type Document = Document' Unresolved' Identity
type ConcreteDocument = Document' VoidF Identity
type Unresolved = Unresolved' Identity

deriving instance (Show (u f), Show (HKD f (Document' u f))) => Show (Document' u f)
deriving instance (Eq   (u f), Eq   (HKD f (Document' u f))) => Eq   (Document' u f)
deriving instance Show (HKD f (Document' Unresolved' f)) => Show (Unresolved' f)
deriving instance Eq   (HKD f (Document' Unresolved' f)) => Eq   (Unresolved' f)

instance FFunctor u => FFunctor (Document' u) where
  ffmap f = \case
    Atom       x -> Atom       x
    List       l -> List       $ fmap (ffrecur @(Document' u) f) l
    Struct     s -> Struct     $ ffmap f s
    Unresolved u -> Unresolved $ ffmap f u

instance FFunctor Unresolved' where
  ffmap f = \case
    Top            -> Top
    NotNull        -> NotNull
    Type         x -> Type         x
    Func         x -> Func         x
    IntegerBound x -> IntegerBound x
    FloatBound   x -> FloatBound   x
    StringBound  x -> StringBound  x
    BytesBound   x -> BytesBound   x
    Thunk        x -> Thunk        x
    Disjoint   l s -> Disjoint
      (fmap (ffrecur @(Document' Unresolved') f) l)
      (fmap (ffrecur @(Document' Unresolved') f) s)


--------------------------------------------------------------------------------
-- * Path

data Step
  = FieldName Text
  | ListIndex Int
  deriving (Show, Eq)

type Path = Seq Step


--------------------------------------------------------------------------------
-- * Struct

data StructInfo' u f = StructInfo
  { _sFields     :: HashMap Text (Field' u f)
  , _sAttributes :: I.Attributes
  }

type StructInfo = StructInfo' Unresolved' Identity

deriving instance Show (HKD f (Document' u f)) => Show (StructInfo' u f)
deriving instance Eq   (HKD f (Document' u f)) => Eq   (StructInfo' u f)

instance FFunctor u => FFunctor (StructInfo' u) where
  ffmap f StructInfo {..} =
    StructInfo (fmap (ffmap f) _sFields) _sAttributes


data Field' u f = Field
  { _fValue      :: HKD f (Document' u f)
  , _fAttributes :: I.Attributes
  }

type Field = Field' Unresolved' Identity

deriving instance Show (HKD f (Document' u f)) => Show (Field' u f)
deriving instance Eq   (HKD f (Document' u f)) => Eq   (Field' u f)

instance FFunctor u => FFunctor (Field' u) where
  ffmap f Field {..} =
    Field (ffrecur @(Document' u) f _fValue) _fAttributes


--------------------------------------------------------------------------------
-- * Lenses

makePrisms ''Document'
makePrisms ''Unresolved'
makeLenses ''StructInfo'
makeLenses ''Field'

instance HasDocs (HKD f (Document' u f)) u f => Plated (Document' u f) where
  plate f = \case
    Atom       x -> pure $ Atom x
    List       l -> List   <$> docs f l
    Struct     s -> Struct <$> docs f s
    Unresolved u -> pure $ Unresolved u

instance HasDocs (HKD f (Document' u f)) u f => IndexedPlated Path (Document' u f) where
  indexedPlate path f = \case
    Atom       x -> pure $ Atom x
    List       l -> List   <$> S.traverseWithIndex (\i -> indexedDocs (path :|> ListIndex i) f) l
    Struct     s -> Struct <$> indexedDocs path f s
    Unresolved u -> pure $ Unresolved u

class HasDocs a u f | a -> u f where
  docs :: Traversal' a (Document' u f)
  indexedDocs :: Path -> IndexedTraversal' Path a (Document' u f)

instance HasDocs (Document' u f) u f where
  docs = id
  indexedDocs path fi = indexed fi path

instance HasDocs a u f => HasDocs (Seq a) u f where
  docs = traverse . docs
  indexedDocs p = traverse . indexedDocs p

instance HasDocs a u f => HasDocs (HashMap k a) u f where
  docs = traverse . docs
  indexedDocs p = traverse . indexedDocs p

instance HasDocs (HKD f (Document' u f)) u f => HasDocs (StructInfo' u f) u f where
  docs f (StructInfo fs as) = StructInfo <$> docs f fs <*> pure as
  indexedDocs p f (StructInfo fs as) = do
    ufs <- fs & M.traverseWithKey \name field -> indexedDocs (p :|> FieldName name) f field
    pure $ StructInfo ufs as

instance HasDocs (HKD f (Document' u f)) u f => HasDocs (Field' u f) u f where
  docs f (Field fv as) = Field <$> docs f fv <*> pure as
  indexedDocs p f (Field fv as) = Field <$> indexedDocs p f fv <*> pure as
