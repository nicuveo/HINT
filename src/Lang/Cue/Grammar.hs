module Lang.Cue.Grammar where

import           Control.Monad
import           Control.Monad.Loops        (unfoldM)
import qualified Control.Monad.State        as MS
import           Data.Char
import           Data.Functor               ((<&>))
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty(..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Debug.Trace                (traceM)
import           Text.Megaparsec            hiding (Label, Token, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


--------------------------------------------------------------------------------
-- Identifier

newtype Identifier = Identifier Text
  deriving (Show, Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . fromString

mkIdentifier :: MonadFail m => Text -> m Identifier
mkIdentifier i = do
  let root = fromMaybe i $ T.stripPrefix "_#" i <|> T.stripPrefix "#" i
  case T.uncons root of
    Just (h, t) -> do
      unless (isAlpha h || h == '_') $
        fail "identifier must start with a letter (or modifiers)"
      unless (T.all (\c -> isAlpha c || isDigit c || c == '_') t) $
        fail "identifier body must only contain letters or digits"
      pure $ Identifier i


--------------------------------------------------------------------------------
-- Tokens

data Token
  = TokenIdentifier    Identifier
  | TokenKeyword       Keyword
  | TokenOperator      Operator
  | TokenAttribute     Attribute
  | TokenInterpolation Interpolation
  | TokenString        Text
  | TokenInteger       Integer
  | TokenFloat         Double
  deriving (Show, Eq)

data Keyword
  = KeywordPackage
  | KeywordImport
  | KeywordNull
  | KeywordTrue
  | KeywordFalse
  | KeywordFor
  | KeywordIn
  | KeywordIf
  | KeywordLet
  deriving (Show, Eq, Ord, Enum, Bounded)

data Operator
  = OperatorRealComma
  | OperatorNewlineComma
  | OperatorEOFComma
  | OperatorAdd
  | OperatorSub
  | OperatorMul
  | OperatorPow
  | OperatorQuo
  | OperatorArrow
  | OperatorLAnd
  | OperatorLOr
  | OperatorAnd
  | OperatorOr
  | OperatorEqual
  | OperatorNotEqual
  | OperatorMatch
  | OperatorNotMatch
  | OperatorLTE
  | OperatorGTE
  | OperatorLT
  | OperatorGT
  | OperatorBind
  | OperatorIsA
  | OperatorColon
  | OperatorOption
  | OperatorNot
  | OperatorEllipsis
  | OperatorPeriod
  | OperatorBottom
  | OperatorParensOpen
  | OperatorParensClose
  | OperatorBracesOpen
  | OperatorBracesClose
  | OperatorBracketsOpen
  | OperatorBracketsClose
  deriving (Show, Eq, Ord, Enum, Bounded)

data Attribute = Attribute
  { attributeName   :: Identifier
  , attributeTokens :: [AttributeToken]
  }
  deriving (Show, Eq)

data AttributeToken
  = AttributeToken    Token
  | AttributeParens   [AttributeToken]
  | AttributeBraces   [AttributeToken]
  | AttributeBrackets [AttributeToken]
  deriving (Show, Eq)

type Interpolation = [InterpolationElement]

data InterpolationElement
  = InterpolationString     Text
  | InterpolationExpression Expression
  deriving (Show, Eq)


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
  { fieldLabels     :: NonEmpty Label
  , fieldExpression :: AliasedExpression
  , fieldAttributes :: [Attribute]
  } deriving (Show, Eq)

data Label = Label
  { labelIdentifier :: Maybe Identifier
  , labelExpression :: LabelExpression
  } deriving (Show, Eq)

data LabelExpression
  = LabelName  Optional LabelName
  | LabelAlias AliasedExpression
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

data UnaryExpression
  = UnaryExpression Operator UnaryExpression
  | PrimaryExpression PrimaryExpression
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

type StringLiteral = Either Interpolation Text

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
