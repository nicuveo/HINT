{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Tokens where

import "this" Prelude


--------------------------------------------------------------------------------
-- * Token hierarchy

data Token f
  = TokenIdentifier    (f Identifier)
  | TokenKeyword       (f Keyword)
  | TokenOperator      (f Operator)
  | TokenAttribute     (f Attribute)
  | TokenInterpolation (f (Interpolation [Token f]))
  | TokenString        (f Text)
  | TokenInteger       (f Integer)
  | TokenFloat         (f Double)

deriving instance
  ( Show (f Identifier)
  , Show (f Keyword)
  , Show (f Operator)
  , Show (f Attribute)
  , Show (f (Interpolation [Token f]))
  , Show (f Text)
  , Show (f Integer)
  , Show (f Double)
  ) => Show (Token f)

deriving instance
  ( Eq (f Identifier)
  , Eq (f Keyword)
  , Eq (f Operator)
  , Eq (f Attribute)
  , Eq (f (Interpolation [Token f]))
  , Eq (f Text)
  , Eq (f Integer)
  , Eq (f Double)
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

type Interpolation a = [InterpolationElement a]

data InterpolationElement a
  = InterpolationString     Text
  | InterpolationExpression a
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Token transformations

tmap :: Functor f => (forall a. f a -> g a) -> Token f -> Token g
tmap f = \case
  TokenIdentifier    x -> TokenIdentifier    (f x)
  TokenKeyword       x -> TokenKeyword       (f x)
  TokenOperator      x -> TokenOperator      (f x)
  TokenAttribute     x -> TokenAttribute     (f x)
  TokenInterpolation x -> TokenInterpolation (f $ fmap (map timap) x)
  TokenString        x -> TokenString        (f x)
  TokenInteger       x -> TokenInteger       (f x)
  TokenFloat         x -> TokenFloat         (f x)
  where
    timap = \case
      InterpolationString     s -> InterpolationString s
      InterpolationExpression e -> InterpolationExpression $ map (tmap f) e
