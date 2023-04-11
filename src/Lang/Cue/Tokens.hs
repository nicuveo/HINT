{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Tokens where

import "this" Prelude

import Lang.Cue.HKD


--------------------------------------------------------------------------------
-- * Token hierarchy

data Token f
  = TokenIdentifier             (HKD f Identifier)
  | TokenKeyword                (HKD f Keyword)
  | TokenOperator               (HKD f Operator)
  | TokenAttribute              (HKD f Attribute)
  | TokenString                 (HKD f (String, Text))
  | TokenInteger                (HKD f Integer)
  | TokenFloat                  (HKD f Double)
  | TokenInterpolationBegin     (HKD f ())
  | TokenInterpolationEnd
  | TokenInterpolationExprBegin (HKD f ())
  | TokenInterpolationExprEnd

instance FFunctor Token where
  ffmap f = \case
    TokenIdentifier             x -> TokenIdentifier             (f @Identifier     x)
    TokenKeyword                x -> TokenKeyword                (f @Keyword        x)
    TokenOperator               x -> TokenOperator               (f @Operator       x)
    TokenAttribute              x -> TokenAttribute              (f @Attribute      x)
    TokenString                 x -> TokenString                 (f @(String, Text) x)
    TokenInteger                x -> TokenInteger                (f @Integer        x)
    TokenFloat                  x -> TokenFloat                  (f @Double         x)
    TokenInterpolationBegin     x -> TokenInterpolationBegin     (f @()             x)
    TokenInterpolationEnd         -> TokenInterpolationEnd
    TokenInterpolationExprBegin x -> TokenInterpolationExprBegin (f @()             x)
    TokenInterpolationExprEnd     -> TokenInterpolationExprEnd

instance
  ( Show (HKD f Identifier)
  , Show (HKD f Keyword)
  , Show (HKD f Operator)
  , Show (HKD f Attribute)
  , Show (HKD f (String, Text))
  , Show (HKD f Integer)
  , Show (HKD f Double)
  , Show (HKD f String)
  ) => Show (Token f) where
  show = \case
    TokenIdentifier             i -> show i
    TokenKeyword                k -> show k
    TokenOperator               o -> show o
    TokenAttribute              a -> show a
    TokenString                 s -> show s
    TokenInteger                i -> show i
    TokenFloat                  f -> show f
    TokenInterpolationBegin     _ -> "InterpolationBegin"
    TokenInterpolationExprBegin _ -> "InterpolationExprBegin"
    TokenInterpolationEnd         -> "InterpolationEnd"
    TokenInterpolationExprEnd     -> "InterpolationExprEnd"

deriving instance
  ( Eq (HKD f Identifier)
  , Eq (HKD f Keyword)
  , Eq (HKD f Operator)
  , Eq (HKD f Attribute)
  , Eq (HKD f (String, Text))
  , Eq (HKD f Integer)
  , Eq (HKD f Double)
  , Eq (HKD f ())
  ) => Eq (Token f)

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

newtype Identifier = Identifier { getIdentifier :: Text }
  deriving newtype (Show, Eq, Ord, IsString)

data Attribute = Attribute
  { attributeName :: Identifier
  , attributeText :: Text
  }
  deriving (Show, Eq)
