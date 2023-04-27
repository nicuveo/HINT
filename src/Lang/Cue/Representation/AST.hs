module Lang.Cue.Representation.AST where

import "this" Prelude

import Data.Scientific

import Lang.Cue.Representation.Tokens


--------------------------------------------------------------------------------
-- AST

data SourceFile = SourceFile
  { moduleName         :: Maybe Identifier
  , moduleAttributes   :: [Attribute]
  , moduleImports      :: [Import]
  , moduleDeclarations :: [Declaration]
  } deriving (Show, Eq)

data Import = Import
  { importName :: Maybe Identifier
  , importPath :: Text
  } deriving (Show, Eq)

data Declaration
  = DeclarationField     Field
  | DeclarationEllipsis  Ellipsis
  | DeclarationEmbedding Embedding
  | DeclarationLetClause LetClause
  | DeclarationAttribute Attribute
  deriving (Show, Eq)

data Field = Field
  { fieldLabel      :: Label
  , fieldExpression :: AliasedExpression
  , fieldAttributes :: [Attribute]
  } deriving (Show, Eq)

data Label = Label
  { labelAlias      :: Maybe Identifier
  , labelExpression :: LabelExpression
  } deriving (Show, Eq)

data LabelExpression
  = LabelString     StringLiteral Optional
  | LabelIdentifier Identifier Optional
  | LabelConstraint AliasedExpression
  deriving (Show, Eq)

data Optional
  = Optional
  | Required
  deriving (Show, Eq)

instance Semigroup Optional where
  Optional <> o = o
  _ <> _        = Required

instance Monoid Optional where
  mempty = Optional

type LabelName = Text

type Ellipsis = Maybe Expression

data Embedding
  = EmbeddedComprehension Comprehension
  | EmbeddedExpression    AliasedExpression
  deriving (Show, Eq)

data LetClause = LetClause
  { letName       :: Identifier
  , letExpression :: Expression
  } deriving (Show, Eq)

data AliasedExpression = AliasedExpression
  { aeAlias      :: Maybe Identifier
  , aeExpression :: Expression
  } deriving (Show, Eq)

data Comprehension = Comprehension
  { compClauses :: NonEmpty ComprehensionClause
  , compResult  :: [Declaration]
  } deriving (Show, Eq)

data ComprehensionClause
  = ComprehensionFor        Identifier            Expression
  | ComprehensionIndexedFor Identifier Identifier Expression
  | ComprehensionIf                               Expression
  | ComprehensionLet        Identifier            Expression
  deriving (Show, Eq)

data Expression
  = Unary          UnaryExpression
  | Multiplication Expression Expression
  | Division       Expression Expression
  | Addition       Expression Expression
  | Subtraction    Expression Expression
  | Equal          Expression Expression
  | NotEqual       Expression Expression
  | Match          Expression Expression
  | NotMatch       Expression Expression
  | LessThan       Expression Expression
  | LessOrEqual    Expression Expression
  | GreaterThan    Expression Expression
  | GreaterOrEqual Expression Expression
  | LogicalAnd     Expression Expression
  | LogicalOr      Expression Expression
  | Unification    Expression Expression
  | Disjunction    Expression Expression
  deriving (Show, Eq)

data UnaryExpression = UnaryExpression
  { ueOperators         :: [Operator]
  , uePrimaryExpression :: PrimaryExpression
  }
  deriving (Show, Eq)

data PrimaryExpression
  = PrimaryOperand  Operand
  | PrimarySelector PrimaryExpression (Either Identifier Text)
  | PrimaryIndex    PrimaryExpression Expression
  | PrimarySlice    PrimaryExpression (Maybe Expression) (Maybe Expression)
  | PrimaryCall     PrimaryExpression [Expression]
  deriving (Show, Eq)

data Operand
  = OperandLiteral    Literal
  | OperandName       Identifier
  | OperandExpression Expression
  deriving (Show, Eq)

type StringLiteral = [StringElement]

data StringElement
  = RawStringLiteral Text
  | Interpolation Expression
  deriving (Show, Eq)

data Literal
  = IntegerLiteral Integer
  | FloatLiteral Scientific
  | StringLiteral TextInfo StringLiteral
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
