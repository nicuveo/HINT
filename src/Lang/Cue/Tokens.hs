{-# LANGUAGE UndecidableInstances #-}

module Lang.Cue.Tokens where

import "this" Prelude

import Lang.Cue.Internal.HKD
import Lang.Cue.Internal.NoShow
import Lang.Cue.Location


--------------------------------------------------------------------------------
-- * Token hierarchy

data Token f
  = TokenIdentifier             (HKD f Identifier)
  | TokenKeyword                (HKD f Keyword)
  | TokenOperator               (HKD f Operator)
  | TokenAttribute              (HKD f Attribute)
  | TokenString                 (HKD f (TextInfo, Text))
  | TokenInteger                (HKD f Integer)
  | TokenFloat                  (HKD f Double)
  | TokenInterpolationBegin     (HKD f TextInfo)
  | TokenInterpolationEnd       (HKD f TextInfo)
  | TokenInterpolationExprBegin (HKD f TextInfo)
  | TokenInterpolationExprEnd   (HKD f TextInfo)

instance FFunctor Token where
  ffmap f = \case
    TokenIdentifier             x -> TokenIdentifier             (ffapply @Identifier       f x)
    TokenKeyword                x -> TokenKeyword                (ffapply @Keyword          f x)
    TokenOperator               x -> TokenOperator               (ffapply @Operator         f x)
    TokenAttribute              x -> TokenAttribute              (ffapply @Attribute        f x)
    TokenString                 x -> TokenString                 (ffapply @(TextInfo, Text) f x)
    TokenInteger                x -> TokenInteger                (ffapply @Integer          f x)
    TokenFloat                  x -> TokenFloat                  (ffapply @Double           f x)
    TokenInterpolationBegin     x -> TokenInterpolationBegin     (ffapply @TextInfo         f x)
    TokenInterpolationEnd       x -> TokenInterpolationEnd       (ffapply @TextInfo         f x)
    TokenInterpolationExprBegin x -> TokenInterpolationExprBegin (ffapply @TextInfo         f x)
    TokenInterpolationExprEnd   x -> TokenInterpolationExprEnd   (ffapply @TextInfo         f x)

instance
  ( Show (HKD f Identifier)
  , Show (HKD f Keyword)
  , Show (HKD f Operator)
  , Show (HKD f Attribute)
  , Show (HKD f (TextInfo, Text))
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
    TokenInterpolationBegin     x -> show $ hmap @f @TextInfo (const $ NoShow "InterpolationBegin")     x
    TokenInterpolationEnd       x -> show $ hmap @f @TextInfo (const $ NoShow "InterpolationEnd")       x
    TokenInterpolationExprBegin x -> show $ hmap @f @TextInfo (const $ NoShow "InterpolationExprBegin") x
    TokenInterpolationExprEnd   x -> show $ hmap @f @TextInfo (const $ NoShow "InterpolationExprEnd")   x

deriving instance
  ( Eq (HKD f Identifier)
  , Eq (HKD f Keyword)
  , Eq (HKD f Operator)
  , Eq (HKD f Attribute)
  , Eq (HKD f (TextInfo, Text))
  , Eq (HKD f Integer)
  , Eq (HKD f Double)
  , Eq (HKD f TextInfo)
  ) => Eq (Token f)

instance
  ( HasOffset (HKD f Identifier)
  , HasOffset (HKD f Keyword)
  , HasOffset (HKD f Operator)
  , HasOffset (HKD f Attribute)
  , HasOffset (HKD f (TextInfo, Text))
  , HasOffset (HKD f Integer)
  , HasOffset (HKD f Double)
  , HasOffset (HKD f TextInfo)
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

data TextInfo = TextInfo
  { tiType :: TextType
  , tiHash :: Int
  }
  deriving (Show, Eq)

data TextType
  = SingleLineString
  | SingleLineBytes
  | MultiLinesString
  | MultiLinesBytes
  deriving (Show, Eq)
