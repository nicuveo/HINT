{-# LANGUAGE TemplateHaskell #-}

module Lang.Cue.IR where

import                "this" Prelude

import                Control.Lens      hiding (List)

import                Lang.Cue.AST      qualified as A
import {-# SOURCE #-} Lang.Cue.Document qualified as D
import                Lang.Cue.Tokens   qualified as T


--------------------------------------------------------------------------------
-- * Package

data Package = Package
  { pkgName       :: Text
  , pkgDocument   :: Thunk
  , pkgAttributes :: Attributes
  }


--------------------------------------------------------------------------------
-- * Thunks

-- | A thunk is an unevaluated part of a document, to be evaluated at a later
-- point. It is a "resolved" version of the AST's expression, in which we have
-- resolved and checked functions and identifiers.
data Thunk
  -- binary operations
  = Disjunction    Disjunction
  | Unification    Unification
  | LogicalOr      Thunk Thunk
  | LogicalAnd     Thunk Thunk
  | Equal          Thunk Thunk
  | NotEqual       Thunk Thunk
  | Match          Thunk Thunk
  | NotMatch       Thunk Thunk
  | LessThan       Thunk Thunk
  | LessOrEqual    Thunk Thunk
  | GreaterThan    Thunk Thunk
  | GreaterOrEqual Thunk Thunk
  | Addition       Thunk Thunk
  | Subtraction    Thunk Thunk
  | Multiplication Thunk Thunk
  | Division       Thunk Thunk

  -- unary operations
  | NumId              Thunk
  | Negate             Thunk
  | LogicalNot         Thunk
  | IsNotEqualTo       Thunk
  | Matches            Thunk
  | Doesn'tMatch       Thunk
  | IsLessThan         Thunk
  | IsLessOrEqualTo    Thunk
  | IsGreaterThan      Thunk
  | IsGreaterOrEqualTo Thunk

  -- primary
  | Select Thunk FieldLabel
  | Index  Thunk Thunk
  | Slice  Thunk Thunk Thunk
  | Call   Thunk [Thunk]

  -- groups
  | List  ListInfo
  | Block BlockInfo

  -- interpolation
  | Interpolation T.TextInfo [Thunk]

  -- leaves
  | Type      Type
  | Func      Function
  | Ref       Reference
  | Alias     Path Path FieldLabel
  | Leaf      Atom
  | Top
  | Bottom

  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Structure

-- | We group all disjunctions together, since we need to distinguish between:
--
--     a: *1 | (*2 | 3)   // 1
--     b: *1 |  *2 | 3    // 1 | 2
--
-- We store it as a simple sequence of thunks, with a boolean indicating whether
-- the thunk was labelled as being a default value.
type Disjunction = Seq (Bool, Thunk)

-- | We group all unifications together, for convenience.
type Unification = Seq Thunk


--------------------------------------------------------------------------------
-- * Block

data BlockInfo = BlockInfo
  { _biAbsolutePath :: Path
  , _biAttributes   :: Attributes
  , _biAliases      :: HashMap FieldLabel (Path, Thunk)
  , _biIdentFields  :: HashMap FieldLabel (Seq Field)
  , _biStringFields :: Seq (Thunk, Field)
  , _biEmbeddings   :: Seq Embedding
  , _biConstraints  :: Seq (Thunk, Thunk)
  , _biClosed       :: Bool
  } deriving (Show, Eq)

data Field = Field
  { fieldAlias      :: Maybe FieldLabel
  , fieldValue      :: Thunk
  , fieldOptional   :: A.Optional
  , fieldAttributes :: Attributes
  } deriving (Show, Eq)

-- | We must include the field type in the label to be able to distinguish
-- between fields @"_a"@ and @_a@, which are considered different despite having
-- the exact same text... the difference isn't actually how the field was
-- referred to, @"a"@ and @a@ are considered the same; it's whether the field is
-- exported / visible or not.
data FieldLabel = FieldLabel
  { labelName :: Text
  , labelType :: FieldType
  } deriving (Show, Eq, Ord, Generic)

instance Hashable FieldLabel

data FieldType
  = Regular
  | Definition
  | Hidden
  | HiddenDefinition
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable FieldType


--------------------------------------------------------------------------------
-- * List

data ListInfo = ListInfo
  { listElements   :: [Embedding]
  , listConstraint :: Maybe Thunk
  } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Atom

-- | Leaf values. Null is a "unit", both a type and its only possible value; it
-- therefore does not unify with anything.
data Atom
  = Boolean Bool
  | Integer Integer
  | Float   Double
  | String  Text
  | Bytes   Text
  | Null
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Types

-- | A type is a special value that represents the set of all possible values of
-- that given type. It unifies with atoms of that type, and can be used as a
-- constraint. It only arises from use of the reserved identifiers.
data Type
  = BooleanType
  | NumberType
  | IntegerType
  | FloatType
  | StringType
  | BytesType
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Reference

type Path = Seq PathElem

-- | Not all fields have a valid absolute path; we must keep track of how we
-- reached a field by storing each individual step. Embeddings do not get a path
-- item since their resulting value will be inlined.
data PathElem
  = PathField FieldLabel
  | PathLetClause FieldLabel
  | PathStringField
  | PathConstraint
  deriving (Show, Eq, Generic)

instance Hashable PathElem

-- | To keep track of references, we change every identifier we encounter to be
-- an absolute path instead of a relative path: consider the following example:
--
--     a: {
--       b: number
--       c: {
--         d: b // refers to a.b
--         e: d + 1
--       }
--     }
--     f: a.c & {
--       d: 0
--     }
--
-- if we substitue @a.c@ by its definition in the expression for @f@ and we keep
-- the @b@ reference relative, we do not have a @b@ in scope when evaluating
-- @f@:
--
--     f: {
--       d: b // whoops!
--       e: d + 1
--     } & {
--       d: 0
--     }
--
-- hence keeping an absolute reference to @a.b@ when generating the thunk.
type Reference = Path


--------------------------------------------------------------------------------
-- * Embeddings

data Embedding
  = Comprehension (NonEmpty Clause) BlockInfo
  | InlineThunk   Thunk
  deriving (Show, Eq)

data Clause
  = For FieldLabel Thunk
  | IndexedFor FieldLabel FieldLabel Thunk
  | If Thunk
  | Let FieldLabel Thunk
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Function

-- | All functions are built-ins, since there isn't any syntax in the language
-- to define custom ones. Despite operating on 'Document', they are defined at
-- the IR level, as they are resolved as part of the IR translation and are not
-- part of the resulting document.
data Function = Function
  { functionName :: Text
  , functionBody :: [D.Document] -> Either () D.Document
  }

instance Show Function where
  show (Function n _) = "Function " ++ show n

instance Eq Function where
  (==) = (==) `on` functionName


--------------------------------------------------------------------------------
-- * Attributes

-- | Arbitrary text associated with parts of the document, meant for consumption
-- by programs, ignored when exporting to non-CUE formats.
--
-- The same attribute name can appear more than one in a given scope, and we
-- collect all of them in the order in which they appear.
type Attributes = HashMap Text (Seq Text)


--------------------------------------------------------------------------------
-- * Lens generation
--
-- We put all TH splices at the end of the file since it avoids the drawback of
-- haivng them closer to the relevant type, namely that it makes declaration
-- order within the file matter (which is unpleasant).

makeLenses ''BlockInfo
makePrisms ''Thunk

instance Plated Thunk where
  plate f = \case
    Disjunction        d -> Disjunction        <$> (traverse . _2) f d
    Unification        u -> Unification        <$> traverse f u
    LogicalOr      t1 t2 -> LogicalOr          <$> f t1 <*> f t2
    LogicalAnd     t1 t2 -> LogicalAnd         <$> f t1 <*> f t2
    Equal          t1 t2 -> Equal              <$> f t1 <*> f t2
    NotEqual       t1 t2 -> NotEqual           <$> f t1 <*> f t2
    Match          t1 t2 -> Match              <$> f t1 <*> f t2
    NotMatch       t1 t2 -> NotMatch           <$> f t1 <*> f t2
    LessThan       t1 t2 -> LessThan           <$> f t1 <*> f t2
    LessOrEqual    t1 t2 -> LessOrEqual        <$> f t1 <*> f t2
    GreaterThan    t1 t2 -> GreaterThan        <$> f t1 <*> f t2
    GreaterOrEqual t1 t2 -> GreaterOrEqual     <$> f t1 <*> f t2
    Addition       t1 t2 -> Addition           <$> f t1 <*> f t2
    Subtraction    t1 t2 -> Subtraction        <$> f t1 <*> f t2
    Multiplication t1 t2 -> Multiplication     <$> f t1 <*> f t2
    Division       t1 t2 -> Division           <$> f t1 <*> f t2
    NumId              t -> NumId              <$> f t
    Negate             t -> Negate             <$> f t
    LogicalNot         t -> LogicalNot         <$> f t
    IsNotEqualTo       t -> IsNotEqualTo       <$> f t
    Matches            t -> Matches            <$> f t
    Doesn'tMatch       t -> Doesn'tMatch       <$> f t
    IsLessThan         t -> IsLessThan         <$> f t
    IsLessOrEqualTo    t -> IsLessOrEqualTo    <$> f t
    IsGreaterThan      t -> IsGreaterThan      <$> f t
    IsGreaterOrEqualTo t -> IsGreaterOrEqualTo <$> f t
    Select         t   l -> Select             <$> f t <*> pure l
    Index          t   i -> Index              <$> f t <*> f i
    Slice          t i j -> Slice              <$> f t <*> f i <*> f j
    Call           t   a -> Call               <$> f t <*> traverse f a
    Interpolation    i l -> Interpolation i    <$> traverse f l
    List               l -> List               <$> thunks f l
    Block              b -> Block              <$> thunks f b
    Type               t -> pure $ Type  t
    Func               z -> pure $ Func  z
    Ref                r -> pure $ Ref   r
    Alias          l p n -> pure $ Alias l p n
    Leaf               a -> pure $ Leaf  a
    Top                  -> pure Top
    Bottom               -> pure Bottom

class HasThunks a where
  thunks :: Traversal' a Thunk

instance HasThunks Thunk where
  thunks = id

instance HasThunks a => HasThunks [a] where
  thunks = traverse . thunks

instance HasThunks a => HasThunks (Seq a) where
  thunks = traverse . thunks

instance HasThunks a => HasThunks (Maybe a) where
  thunks = traverse . thunks

instance HasThunks a => HasThunks (NonEmpty a) where
  thunks = traverse . thunks

instance HasThunks a => HasThunks (HashMap k a) where
  thunks = traverse . thunks

instance (HasThunks a, HasThunks b) => HasThunks (a, b) where
  thunks = beside thunks thunks

instance HasThunks BlockInfo where
  thunks f BlockInfo {..} = BlockInfo
    <$> pure _biAbsolutePath
    <*> pure _biAttributes
    <*> (traverse . traverse) f _biAliases
    <*> thunks f _biIdentFields
    <*> thunks f _biStringFields
    <*> thunks f _biEmbeddings
    <*> thunks f _biConstraints
    <*> pure _biClosed

instance HasThunks Field where
  thunks f Field {..} = Field
    <$> pure fieldAlias
    <*> f fieldValue
    <*> pure fieldOptional
    <*> pure fieldAttributes

instance HasThunks ListInfo where
  thunks f ListInfo {..} = ListInfo
    <$> thunks f listElements
    <*> thunks f listConstraint

instance HasThunks Embedding where
  thunks f = \case
    InlineThunk t -> InlineThunk
      <$> f t
    Comprehension c b -> Comprehension
      <$> thunks f c
      <*> thunks f b

instance HasThunks Clause where
  thunks f = \case
    For          x t -> For          x <$> f t
    IndexedFor i x t -> IndexedFor i x <$> f t
    If             t -> If             <$> f t
    Let          l t -> Let          l <$> f t
