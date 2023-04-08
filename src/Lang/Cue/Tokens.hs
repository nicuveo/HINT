module Lang.Cue.Tokens where

import "this" Prelude

import Control.Applicative
import Data.Char
import Data.Text           qualified as T


--------------------------------------------------------------------------------
-- * Identifier

newtype Identifier = Identifier Text
  deriving (Show, Eq, Ord, IsString)

mkIdentifier :: MonadFail m => Text -> m Identifier
mkIdentifier i = do
  let root = fromMaybe i $ T.stripPrefix "_#" i <|> T.stripPrefix "#" i
  case T.uncons root of
    Nothing -> fail "identifier cannot be empty"
    Just (h, t) -> do
      unless (isAlpha h || h == '_') $
        fail "identifier must start with a letter (or modifiers)"
      unless (T.all (\c -> isAlpha c || isDigit c || c == '_') t) $
        fail "identifier body must only contain letters or digits"
      pure $ Identifier i


--------------------------------------------------------------------------------
-- * Tokens

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
  { attributeName :: Identifier
  , attributeText :: Text
  }
  deriving (Show, Eq)

type Interpolation = [InterpolationElement]

data InterpolationElement
  = InterpolationString     Text
  | InterpolationExpression [Token]
  deriving (Show, Eq)
