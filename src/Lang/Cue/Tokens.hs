{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Tokens where

import "this" Prelude

import Lang.Cue.HKD
import Lang.Cue.Location
import Lang.Cue.NoShow


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
  | TokenInterpolationEnd       (HKD f ())
  | TokenInterpolationExprBegin (HKD f ())
  | TokenInterpolationExprEnd   (HKD f ())

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
    TokenInterpolationEnd       x -> TokenInterpolationEnd       (f @()             x)
    TokenInterpolationExprBegin x -> TokenInterpolationExprBegin (f @()             x)
    TokenInterpolationExprEnd   x -> TokenInterpolationExprEnd   (f @()             x)

instance
  ( Show (HKD f Identifier)
  , Show (HKD f Keyword)
  , Show (HKD f Operator)
  , Show (HKD f Attribute)
  , Show (HKD f (String, Text))
  , Show (HKD f Integer)
  , Show (HKD f Double)
  , Show (HKD f NoShow)
  , HKDF f
  ) => Show (Token f) where
  show = \case
    TokenIdentifier             i -> show i
    TokenKeyword                k -> show k
    TokenOperator               o -> show o
    TokenAttribute              a -> show a
    TokenString                 s -> show s
    TokenInteger                i -> show i
    TokenFloat                  f -> show f
    TokenInterpolationBegin     x -> show $ hmap @f @() (const $ NoShow "InterpolationBegin")     x
    TokenInterpolationEnd       x -> show $ hmap @f @() (const $ NoShow "InterpolationEnd")       x
    TokenInterpolationExprBegin x -> show $ hmap @f @() (const $ NoShow "InterpolationExprBegin") x
    TokenInterpolationExprEnd   x -> show $ hmap @f @() (const $ NoShow "InterpolationExprEnd")   x

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

instance
  ( HasOffset (HKD f Identifier)
  , HasOffset (HKD f Keyword)
  , HasOffset (HKD f Operator)
  , HasOffset (HKD f Attribute)
  , HasOffset (HKD f (String, Text))
  , HasOffset (HKD f Integer)
  , HasOffset (HKD f Double)
  , HasOffset (HKD f ())
  ) => HasOffset (Token f) where
  getOffset = \case
    TokenIdentifier             i -> getOffset i
    TokenKeyword                k -> getOffset k
    TokenOperator               o -> getOffset o
    TokenAttribute              a -> getOffset a
    TokenString                 s -> getOffset s
    TokenInteger                i -> getOffset i
    TokenFloat                  f -> getOffset f
    TokenInterpolationBegin     x -> getOffset x
    TokenInterpolationExprBegin x -> getOffset x
    TokenInterpolationEnd       x -> getOffset x
    TokenInterpolationExprEnd   x -> getOffset x

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
