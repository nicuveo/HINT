{-# LANGUAGE TemplateHaskell #-}

module Lang.Cue.IR where

import                "this" Prelude

import                Control.Lens

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
  | Alias     Text
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
  { _biAttributes   :: Attributes
  , _biAliases      :: HashMap FieldLabel (Maybe Thunk)
  , _biIdentFields  :: HashMap FieldLabel (Seq Field)
  , _biStringFields :: Seq (Thunk, Field)
  , _biEmbeddings   :: Seq Embedding
  , _biConstraints  :: Seq (Thunk, Thunk)
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
-- However! We *also* need to keep a relative path so that we can do reference
-- substitution. If, in the previous example, we inline the definition of @c@
-- using absolute paths but without altering its inner references, we get:
--
--     f: {
--       d: a.b
--       e: a.c.d + 1 // whoops!
--     } & {
--       d: 0
--     }
--
-- so we keep *both*! When we inline @a.c@ in the definition of @f@, we can keep
-- all "outer" references, and replace all the inner ones, to finally obtain:
--
--     f: {
--       d: a.b     // points back to the "outer" original value
--       e: f.d + 1 // points to the new local field
--     } & {
--       d: 0
--     }
--
-- in practice, we don't need to keep the full relative path; we just need to
-- know how many "steps up" are required before finding a common path between
-- the "referent" and the "referee".
data Reference = Reference
  { refPath  :: [FieldLabel]
  , refSteps :: Int
  }
  deriving (Show, Eq)


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
-- to define custom ones.
data Function = Function
  { functionName :: Text
  , functionBody :: [D.Document] -> D.Document
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
-- Lens generation
--
-- We put all TH splices at the end of the file since it avoids the drawback of
-- haivng them closer to the relevant type, namely that it makes declaration
-- order within the file matter (which is unpleasant).

makeLenses ''BlockInfo
