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
  = Atom      Atom
  | List      (ListInfo Document)
  | Struct    Struct
  -- conditions
  | NotNull
  | BoolBound    (Bound Bool    Void    Void)
  | IntegerBound (Bound Integer Integer Void)
  | FloatBound   (Bound Float   Float   Void)
  | StringBound  (Bound Text    Text    Text)
  | BytesBound   (Bound Text    Text    Void)
  | Type         Type
  -- unevaluated
  | Func      Function
  | DocThunk  Thunk
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

-- | Bound for an ordered value.
data EndPoint o
  = Open
  | Inclusive o
  | Exclusive o
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
-- * Function

-- | All functions are built-ins, since there isn't any syntax in the language
-- to define custom ones.
data Function = Function
  { functionName :: Text
  , functionBody :: [Document] -> Document
  }

instance Show Function where
  show (Function n _) = "Function " ++ show n

instance Eq Function where
  (==) = (==) `on` functionName


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
--
-- The structure of a list being the same for concrete and unresolved values,
-- this type uses the @v@ type parameter to choose what kind of data the list
-- contains.
data ListInfo v = ListInfo
  { listElements   :: [v]
  , listConstraint :: Maybe v
  } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Struct

-- | Mapping from identifiers to sub-documents.
--
-- A struct's fields can only be resolved when all embeds are concrete, since
-- they will resolve in fields being added to the struct. But even when we have
-- the list of fields, we need to keep the list of locally declared expressions
-- so that we can finish evaluating thunks in sub-fields.
--
-- The structure of a struct being the same for concrete and unresolved values,
-- this type uses the @v@ type parameter to choose what kind of data the list
-- contains, and use the @e@ paran for unresolved embeddings.
data Definitions v e = Definitions
  { defFields      :: HashMap FieldLabel (Field v)
  , defConstraints :: [(v, v)]
  , defAliases     :: HashMap Text v
  , defAttributes  :: Attributes
  , defEmbeddings  :: [e]
  , defCanBeAtom   :: Bool
  } deriving (Show, Eq)

type Struct = Definitions Document Void
type Block  = Definitions Thunk    Embedding

data Field v = Field
  { fieldAlias      :: Maybe Text
  , fieldValue      :: v
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
  | ListLiteral (ListInfo Thunk)
  | Block       Block

  -- leaves
  | Package   Text
  | Reference FieldLabel
  | Leaf      Atom
  | Top
  | Bottom

  deriving (Show, Eq)

-- | We group all disjunctions together, since we need to distinguish between:
--
--     a: *1 | (*2 | 3)   // 1
--     b: *1 |  *2 | 3    // 1 | 2
--
-- We store it as a simple sequence of thunks, with a boolean indicating whether
-- the thunk was labelled as being a default value.
type Disjunction = Seq (Bool, Thunk)


--------------------------------------------------------------------------------
-- * Embeddings

data Embedding
  = Comprehension (NonEmpty Clause) Block
  | InlineThunk   Thunk
  deriving (Show, Eq)

data Clause
  = For        Text      Thunk
  | IndexedFor Text Text Thunk
  | If                   Thunk
  | Let        Text      Thunk
  deriving (Show, Eq)
