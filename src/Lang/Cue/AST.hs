module Lang.Cue.AST where

import "this" Prelude

import Lang.Cue.Tokens


--------------------------------------------------------------------------------
-- AST

data SourceFile = SourceFile
  { moduleName         :: Maybe Identifier
  , moduleAttributes   :: [Attribute]
  , moduleImports      :: [Import]
  , moduleDeclarations :: [Declaration]
  } deriving (Show, Eq)

data Import = Import
  { importName  :: Maybe Identifier
  , importPath  :: Text
  , importIdent :: Maybe Identifier
  } deriving (Show, Eq)

data Declaration
  = DeclarationField     Field
  | DeclarationEllipsis  Ellipsis
  | DeclarationEmbedding Embedding
  | DeclarationLetClause LetClause
  | DeclarationAttribute Attribute
  deriving (Show, Eq)

data Field = Field
  { fieldLabels     :: Label
  , fieldExpression :: AliasedExpression
  , fieldAttributes :: [Attribute]
  } deriving (Show, Eq)

data Label = Label
  { labelAlias      :: Maybe Identifier
  , labelExpression :: LabelExpression
  } deriving (Show, Eq)

data LabelExpression
  = LabelIdentifier Identifier    Optional
  | LabelString     StringLiteral Optional
  | LabelConstraint AliasedExpression
  deriving (Show, Eq)

data Optional
  = Optional
  | Required
  deriving (Show, Eq)

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
  | Multiplication Expression Expression [Expression]
  | Division       Expression Expression [Expression]
  | Addition       Expression Expression [Expression]
  | Subtraction    Expression Expression [Expression]
  | Equal          Expression Expression [Expression]
  | NotEqual       Expression Expression [Expression]
  | Match          Expression Expression [Expression]
  | NotMatch       Expression Expression [Expression]
  | LessThan       Expression Expression [Expression]
  | LessOrEqual    Expression Expression [Expression]
  | GreaterThan    Expression Expression [Expression]
  | GreaterOrEqual Expression Expression [Expression]
  | LogicalAnd     Expression Expression [Expression]
  | LogicalOr      Expression Expression [Expression]
  | Unification    Expression Expression [Expression]
  | Disjunction    Expression Expression [Expression]
  deriving (Show, Eq)

data UnaryExpression = UnaryExpression
  { ueOperators         :: [Operator]
  , uePrimaryExpression :: PrimaryExpression
  }
  deriving (Show, Eq)

data PrimaryExpression
  = PrimaryOperand  Operand
  | PrimarySelector PrimaryExpression (Either Identifier StringLiteral)
  | PrimaryIndex    PrimaryExpression Expression
  | PrimarySlice    PrimaryExpression (Expression, Expression)
  | PrimaryCall     PrimaryExpression [Expression]
  deriving (Show, Eq)

data Operand
  = OperandLiteral    Literal
  | OperandName       QualifiedIdentifier
  | OperandExpression Expression
  deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier
  { qiPackageName :: Maybe Identifier
  , qiIdentifier  :: Identifier
  } deriving (Show, Eq)

type StringLiteral = Either (Interpolation Expression) Text

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
