{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import           Control.Monad
import           Data.List.NonEmpty    (NonEmpty(..))
import qualified Data.List.NonEmpty    as NE
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Test.Tasty.QuickCheck

import           Lang.Cue.Grammar


-- arbitrary instances

instance Arbitrary Token where
  arbitrary = sized \s -> oneof $ concat
    [ [ TokenIdentifier <$> arbitrary
      , TokenKeyword    <$> arbitrary
      , TokenOperator   <$> arbitraryTokenOperator
      , TokenString     <$> arbitraryText
      , TokenInteger . getPositive <$> arbitrary
      , TokenFloat   . getPositive <$> arbitrary
      ]
    , guard (s > 0) *>
      [ TokenAttribute <$> arbitrary
      ]
    ]

instance Arbitrary Identifier where
  arbitrary = do
    let
      letter   = '_' : ['a'..'z'] ++ ['A'..'Z']
      digit    = ['0'..'9']
      alphaNum = letter ++ digit
    m <- elements ["", "#", "_#"]
    h <- elements letter
    t <- listOf $ elements alphaNum
    pure $ Identifier $ T.pack (m ++ h:t)

instance Arbitrary Keyword where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Attribute where
  arbitrary = Attribute <$> arbitrary <*> arbitrary

instance Arbitrary AttributeToken where
  arbitrary = sized \s -> frequency
    [ (6,       AttributeToken    <$> scale (`div` 2) arbitrary)
    , (min 1 s, AttributeParens   <$> scale (`div` 5) arbitrary)
    , (min 1 s, AttributeBraces   <$> scale (`div` 5) arbitrary)
    , (min 1 s, AttributeBrackets <$> scale (`div` 5) arbitrary)
    ]

instance Arbitrary SourceFile where
  arbitrary = SourceFile
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Import where
  arbitrary = Import
    <$> arbitrary
    <*> arbitraryImport
    <*> arbitrary

instance Arbitrary Declaration where
  arbitrary = oneof
    [ DeclarationEllipsis  <$> arbitrary
    , DeclarationEmbedding <$> arbitrary
    , DeclarationLetClause <$> arbitrary
    , DeclarationAttribute <$> arbitrary
    ]

instance Arbitrary Field where
  arbitrary = Field
    <$> arbitraryNonEmpty
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Label where
  arbitrary = Label
    <$> arbitrary
    <*> arbitrary

instance Arbitrary LabelExpression where
  arbitrary = oneof
    [ LabelName  <$> arbitrary <*> arbitraryText
    , LabelAlias <$> arbitrary
    ]

instance Arbitrary Optional where
  arbitrary = elements [Optional, Required]

instance Arbitrary Embedding where
  arbitrary = oneof
    [ EmbeddedExpression    <$> arbitrary
    , EmbeddedComprehension <$> arbitrary
    ]

instance Arbitrary LetClause where
  arbitrary = LetClause
    <$> arbitrary
    <*> arbitrary

instance Arbitrary AliasedExpression where
  arbitrary = AliasedExpression
    <$> arbitrary
    <*> arbitrary

instance Arbitrary Comprehension where
  arbitrary = Comprehension
    <$> ((:|) <$> firstFor <*> arbitrary)
    <*> arbitrary
    where
      firstFor = ComprehensionFor <$> arbitrary <*> arbitrary

instance Arbitrary ComprehensionClause where
  arbitrary = oneof
    [ ComprehensionFor        <$> arbitrary               <*> arbitrary
    , ComprehensionIndexedFor <$> arbitrary <*> arbitrary <*> arbitrary
    , ComprehensionIf                                     <$> arbitrary
    , ComprehensionLet        <$> arbitrary               <*> arbitrary
    ]

instance Arbitrary Expression where
  arbitrary = sized \n -> e ops (min 3 n)
    where
      ops =
        [ -- precedence 1
          [ Disjunction
          ]
        , -- precedence 2
          [ Unification
          ]
        , -- precedence 3
          [ LogicalOr
          ]
        , -- precedence 4
          [ LogicalAnd
          ]
        , -- precedence 5
          [ Equal
          , NotEqual
          , Match
          , NotMatch
          , LessOrEqual
          , LessThan
          , GreaterOrEqual
          , GreaterThan
          ]
        , -- precedence 6
          [ Addition
          , Subtraction
          ]
        , -- precedence 7
          [ Multiplication
          , Division
          ]
        ]
      e _  0 = Unary <$> arbitrary
      e [] _ = Unary <$> arbitrary
      e l  d = do
        i <- chooseInt (0, length l)
        case drop i l of
          []    -> Unary <$> arbitrary
          (x:r) -> do
            o <- elements x
            o <$> e r (d-1) <*> e r (d-1)

instance Arbitrary UnaryExpression where
  arbitrary = sized ue
    where
      ue n = do
        ops <- chooseInt (0, n `div` 2)
        elt <- PrimaryExpression <$> arbitrary
        go ops elt
      go 0 e = pure e
      go n e = do
        op <- arbitraryUnaryOperator
        go (n-1) $ UnaryExpression op e

instance Arbitrary PrimaryExpression where
  arbitrary = scale (`div` 5) do
    op <- PrimaryOperand <$> arbitrary
    s  <- getSize
    n  <- chooseInt (0,s)
    go n op
    where
      go 0 pe = pure pe
      go n pe = go (n-1) =<< oneof
        -- it is not possible to differentiate at the parser level
        -- between selector and qualified identifier: both have the
        -- shape @identifier "." identifier@.
        [ PrimaryIndex pe <$> arbitrary
        , PrimarySlice pe <$> arbitrary
        , PrimaryCall  pe <$> arbitrary
        ]

instance Arbitrary Operand where
  arbitrary = sized o
    where
      o n = frequency
        [ (2,       OperandLiteral    <$> scale (`div` 2) arbitrary)
        , (2,       OperandName       <$> scale (`div` 2) arbitrary)
        , (min 1 n, OperandExpression <$> scale (`div` 2) arbitrary)
        ]

instance Arbitrary QualifiedIdentifier where
  arbitrary = QualifiedIdentifier
    <$> arbitrary
    <*> arbitrary

instance Arbitrary Literal where
  arbitrary = sized \s -> oneof $ concat
    [ [ IntegerLiteral . getPositive <$> arbitrary
      , FloatLiteral   . getPositive <$> arbitrary
      , StringLiteral  . Right       <$> arbitraryText
      , BoolLiteral                  <$> arbitrary
      , pure NullLiteral
      , pure BottomLiteral
      ]
    , guard (s>0) *>
      [ StringLiteral . Left  <$> scale (`div` 2) arbitraryInterpolation
      , ListLiteral           <$> scale (`div` 4) arbitrary
      -- , StructLiteral         <$> scale (`div` 2) arbitrary
      ]
    ]

instance Arbitrary ListLiteral where
  arbitrary = oneof
    [ ClosedList <$> arbitrary
    , OpenList   <$> arbitrary <*> arbitrary
    ]


-- helpers

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

arbitraryText1 :: Gen Text
arbitraryText1 = T.pack <$> listOf1 arbitrary

arbitraryImport :: Gen Text
arbitraryImport = T.pack <$> listOf1 (elements ['a'..'z'])

arbitraryNonEmpty :: Arbitrary a => Gen (NonEmpty a)
arbitraryNonEmpty = NE.fromList <$> listOf1 arbitrary

arbitraryInterpolation :: Gen [InterpolationElement]
arbitraryInterpolation = do
  a <- arbitraryText1
  b <- arbitrary
  c <- arbitraryText1
  pure [InterpolationString a, InterpolationExpression b, InterpolationString c]

arbitraryTokenOperator :: Gen Operator
arbitraryTokenOperator = elements [OperatorAdd .. OperatorBottom]

arbitraryUnaryOperator :: Gen Operator
arbitraryUnaryOperator = elements
  [ OperatorAdd
  , OperatorSub
  , OperatorMul
  , OperatorNotEqual
  , OperatorNotMatch
  , OperatorNot
  , OperatorMatch
  , OperatorLTE
  , OperatorLT
  , OperatorGTE
  , OperatorGT
  ]
