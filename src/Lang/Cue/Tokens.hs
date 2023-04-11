{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Tokens where

import "this" Prelude


--------------------------------------------------------------------------------
-- * Token hierarchy

data Token f
  = TokenIdentifier             (f Identifier)
  | TokenKeyword                (f Keyword)
  | TokenOperator               (f Operator)
  | TokenAttribute              (f Attribute)
  | TokenString                 (f (String, Text))
  | TokenInteger                (f Integer)
  | TokenFloat                  (f Double)
  | TokenInterpolationBegin     (f ())
  | TokenInterpolationEnd
  | TokenInterpolationExprBegin (f ())
  | TokenInterpolationExprEnd

deriving instance
  ( Show (f Identifier)
  , Show (f Keyword)
  , Show (f Operator)
  , Show (f Attribute)
  , Show (f (String, Text))
  , Show (f Integer)
  , Show (f Double)
  , Show (f ())
  ) => Show (Token f)

deriving instance
  ( Eq (f Identifier)
  , Eq (f Keyword)
  , Eq (f Operator)
  , Eq (f Attribute)
  , Eq (f (String, Text))
  , Eq (f Integer)
  , Eq (f Double)
  , Eq (f ())
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

newtype Identifier = Identifier Text
  deriving (Show, Eq, Ord, IsString)

data Attribute = Attribute
  { attributeName :: Identifier
  , attributeText :: Text
  }
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Token transformations

tmap :: (forall a. f a -> g a) -> Token f -> Token g
tmap f = \case
  TokenIdentifier             x -> TokenIdentifier             (f x)
  TokenKeyword                x -> TokenKeyword                (f x)
  TokenOperator               x -> TokenOperator               (f x)
  TokenAttribute              x -> TokenAttribute              (f x)
  TokenString                 x -> TokenString                 (f x)
  TokenInteger                x -> TokenInteger                (f x)
  TokenFloat                  x -> TokenFloat                  (f x)
  TokenInterpolationBegin     x -> TokenInterpolationBegin     (f x)
  TokenInterpolationEnd         -> TokenInterpolationEnd
  TokenInterpolationExprBegin x -> TokenInterpolationExprBegin (f x)
  TokenInterpolationExprEnd     -> TokenInterpolationExprEnd
