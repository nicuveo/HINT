{-# LANGUAGE TemplateHaskell #-}

module Lang.Cue.Document where

import "this" Prelude

import Lang.Cue.IR    qualified as I


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
data Document
  -- structure
  = Atom         I.Atom
  | List         [Document]
  | Struct       StructInfo
  -- constraints
  | NotNull
  | BoolBound    BoolBound
  | IntegerBound IntegerBound
  | FloatBound   FloatBound
  | StringBound  StringBound
  | BytesBound   BytesBound
  -- unevaluated
  | Thunk        I.Thunk
  deriving (Show, Eq)


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

data StructInfo = StructInfo
  { structFields     :: HashMap Text Field
  , structAttributes :: I.Attributes
  } deriving (Show, Eq)

data Field = Field
  { fieldValue      :: Document
  , fieldAttributes :: I.Attributes
  } deriving (Show, Eq)
