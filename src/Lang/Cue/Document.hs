module Lang.Cue.Document where

import "this" Prelude


--------------------------------------------------------------------------------
-- * Document

-- | Representation of an interpreted cue document. It is similar to a JSON
-- document, except that any part of it might be left unevaluated, and each
-- struct or field can be annotated.
--
-- Compiling / validating the AST results in an evaluated document; evaluating
-- the document reduces it as much as possible, down to a concrete value if
-- possible, but leaving all unresolved elements as thunks.
--
-- Bottom is not represented as part of the document, since bottom represents
-- (recoverable) evaluation errors.
data Document
  -- structure
  = Atom   Atom
  | Bound  Bound
  | List   List
  | Struct Struct
  -- unevaluated
  | Thunk  Thunk
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Atom

-- | Evaluated leaf values. Null is a "unit", both a type and its only possible
-- value; it therefore does not unify with anything.
data Atom
  = Boolean Bool
  | Integer Integer
  | Float   Double
  | String  Text
  | Bytes   Text
  | Null
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Bounds

-- | Bounds represent conditions on values. They unify with a value by returning
-- the value if it matches the condition, by throwing an error otherwise.
data Bound = Bound
  { -- | lower bound (if any)
    minBound    :: EndPoint
  , -- | upper bound (if any)
    maxBound    :: EndPoint
  , -- | excluded values
    different   :: [Document]
  , -- | regex match
    matchesAll  :: [Document]
  , -- | regex non-match
    matchesNone :: [Document]
  }
  deriving (Show, Eq)

-- | Bound for an ordered value.
data EndPoint
  = Open
  | Inclusive Document
  | Exclusive Document
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * List

-- | List of documents.
--
-- Unlike structs, lists do not allow annotations, but they are however very
-- similar: each field of a list can be reduced in parallel, but all fields must
-- be valid for a list to be valid (bottoms will not be caught).
--
-- A list can still be concrete even with an ellipsis: the ellipsis just gets
-- ignored when reifying.
data List = List
  { listElements   :: [Document]
  , listConstraint :: Maybe Document
  } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Struct

-- | Mapping from identifiers to sub-documents.
--
-- A struct's fields can only be resolved when all embeds are concrete, since
-- they will resolve in fields being added to the struct. But even when we have
-- the list of fields, we need to keep the list of locally declared expressions
-- so that we can finish evaluating thunks in sub-fields.
data Struct = Struct
  { structFields      :: HashMap FieldLabel Field
  , structConstraints :: [(Document, Document)]
  , structAliases     :: HashMap Text Document
  , structAttributes  :: Attributes
  } deriving (Show, Eq)

data Field = Field
  { fieldAlias      :: Maybe Text
  , fieldValue      :: Document
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
  } deriving (Show, Eq, Ord, Hashable)

data FieldType
  = Regular
  | Definition
  | Hidden
  | HiddenDefinition
  deriving (Show, Eq, Ord, Hashable)


--------------------------------------------------------------------------------
-- * Attributes

-- | Arbitrary text associated with parts of the document, meant for consumption
-- by programs, ignored when exporting to non-CUE formats.
--
-- The same attribute name can appear more than one in a given scope, and we
-- collect all of them in the order in which they appear.
type Attributes = HashMap Text [Text]


--------------------------------------------------------------------------------
-- * Thunks

-- | A thunk is an unevaluated part of a document, to be evaluated at a later
-- point. It is a "resolved" version of the AST's expression, in which we have
-- resolved and checked functions and identifiers.
data Thunk
  -- binary operations
  = Disjunction    Disjunction
  | Unification    Thunk Thunk
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
  -- terms
  | NumId  Thunk
  | Negate Thunk
  deriving (Show, Eq)

-- | We group all disjunctions together, since we need to distinguish between:
--
--     a: *1 | (*2 | 3)   // 1
--     b: *1 |  *2 | 3    // 1 | 2
--
-- We store it as a simple sequence of thunks, with a boolean indicating whether
-- the thunk was labelled as being a default value.
type Disjunction = Seq (Bool, Thunk)


-- | Unevaluated term of an expression.
data Term
  = 



data UnaryExpression = UnaryExpression
  { ueOperators         :: [Operator]
  , uePrimaryExpression :: PrimaryExpression
  }
  deriving (Show, Eq)

data PrimaryExpression
  = PrimaryOperand  Operand
  | PrimarySelector PrimaryExpression (Either Identifier Text)
  | PrimaryIndex    PrimaryExpression Expression
  | PrimarySlice    PrimaryExpression (Expression, Expression)
  | PrimaryCall     PrimaryExpression [Expression]
  deriving (Show, Eq)

data Operand
  = OperandLiteral    Literal
  | OperandName       Identifier
  | OperandExpression Expression
  deriving (Show, Eq)

type StringLiteral = [StringElement]

data StringElement
  = RawStringLiteral String Text
  | Interpolation Expression
  deriving (Show, Eq)

data Literal
  = IntegerLiteral Integer
  | FloatLiteral Double
  | StringLiteral StringLiteral
  | BoolLiteral Bool
  | NullLiteral
  | BottomLiteral
  | ListLiteral ListLiteral
  | StructLiteral [Declaration]
  deriving (Show, Eq)

data ListLiteral
  = ClosedList [Embedding]
  | OpenList   [Embedding] Ellipsis
  deriving (Show, Eq)
