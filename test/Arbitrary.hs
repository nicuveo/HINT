{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import "this" Prelude

import Control.Monad
import Data.List.NonEmpty    qualified as NE
import Data.Text             qualified as T
import Test.Tasty.QuickCheck

import Lang.Cue.AST
import Lang.Cue.Error
import Lang.Cue.HKD
import Lang.Cue.Location
import Lang.Cue.Tokens


--------------------------------------------------------------------------------
-- Location

instance Arbitrary a => Arbitrary (WithOffset a) where
  arbitrary = liftA2 withOffset arbitrary arbitrary

instance Arbitrary a => Arbitrary (WithLocation a) where
  arbitrary = liftA2 withLocation arbitrary arbitrary

instance Arbitrary Location where
  arbitrary = Location
    <$> arbitrary
    <*> listOf arbitraryText
    <*> arbitrary


--------------------------------------------------------------------------------
-- Tokens

instance
  ( Arbitrary (HKD f Identifier)
  , Arbitrary (HKD f Keyword)
  , Arbitrary (HKD f Operator)
  , Arbitrary (HKD f Attribute)
  , Arbitrary (HKD f (String, Text))
  , Arbitrary (HKD f (Positive Integer))
  , Arbitrary (HKD f (Positive Double))
  , Arbitrary (HKD f ())
  , Functor f
  ) => Arbitrary (Token f) where
  arbitrary = oneof
    [ TokenInteger . hmap @f @(Positive Integer) getPositive <$> arbitrary
    , TokenFloat   . hmap @f @(Positive Double)  getPositive <$> arbitrary
    , TokenIdentifier <$> arbitrary
    , TokenKeyword    <$> arbitrary
    , TokenOperator   <$> arbitrary
    , TokenAttribute  <$> arbitrary
    , do
        og <- arbitrary :: Gen (HKD f ())
        t  <- arbitraryText
        let delim = "\"" :: String
        pure $ TokenString $ hmap @f @() (const (delim, t)) og
    ]

instance Arbitrary Identifier where
  arbitrary = genName `suchThat` notAKeyword
    where
      genName = do
        let
          letter = ['a'..'z'] ++ ['A'..'Z']
          digit  = ['0'..'9']
          body   = '_' : letter ++ digit
        m <- elements ["", "#", "_#"]
        h <- elements letter
        t <- listOf $ elements body
        pure $ Identifier $ T.pack (m ++ h:t)
      notAKeyword = flip notElem
        [ "package"
        , "import"
        , "null"
        , "true"
        , "false"
        , "for"
        , "in"
        , "if"
        , "let"
        ]

instance Arbitrary Keyword where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Operator where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Attribute where
  arbitrary = Attribute <$> arbitrary <*> arbitraryText


--------------------------------------------------------------------------------
-- File structure

instance Arbitrary SourceFile where
  arbitrary = scale (`div` 2) $ SourceFile
    <$> (Just <$> arbitrary)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink (SourceFile a b c d) = concat
    [ SourceFile a <$> shrink b <*> pure c   <*> pure d
    , SourceFile a <$> pure b   <*> shrink c <*> pure d
    , SourceFile a <$> pure b   <*> pure c   <*> shrink d
    ]

instance Arbitrary Import where
  arbitrary = Import
    <$> arbitrary
    <*> arbitraryImport

instance Arbitrary Declaration where
  arbitrary = oneof
    [ DeclarationField     <$> scale (`div` 2) arbitrary
    , DeclarationEllipsis  <$> scale (`div` 2) arbitrary
    , DeclarationEmbedding <$> scale (`div` 2) arbitrary
    , DeclarationLetClause <$> scale (`div` 2) arbitrary
    , DeclarationAttribute <$> scale (`div` 2) arbitrary
    ]
  shrink = \case
    DeclarationField     d -> DeclarationField     <$> shrink d
    DeclarationEllipsis  d -> DeclarationEllipsis  <$> shrink d
    DeclarationEmbedding d -> DeclarationEmbedding <$> shrink d
    DeclarationLetClause d -> DeclarationLetClause <$> shrink d
    DeclarationAttribute d -> DeclarationAttribute <$> shrink d

instance Arbitrary Field where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink (Field a b c) = concat
    [ Field <$> shrink a <*> pure   b <*> pure   c
    , Field <$> pure   a <*> shrink b <*> pure   c
    , Field <$> pure   a <*> pure   b <*> shrink c
    ]

instance Arbitrary Label where
  arbitrary = Label
    <$> arbitrary
    <*> arbitrary
  shrink (Label a e) = concat
    [ Label <$> shrink a <*> pure   e
    , Label <$> pure   a <*> shrink e
    ]

instance Arbitrary LabelExpression where
  arbitrary = oneof
    [ LabelString     <$> arbitraryText <*> arbitrary
    , LabelIdentifier <$> arbitrary <*> arbitrary
    , LabelConstraint <$> arbitrary
    ]
  shrink = \case
    LabelIdentifier _ _ -> []
    LabelString     _ _ -> []
    LabelConstraint   c -> LabelConstraint <$> shrink c

instance Arbitrary Optional where
  arbitrary = elements [Optional, Required]

instance Arbitrary Embedding where
  arbitrary = oneof
    [ EmbeddedExpression    <$> arbitrary
    , EmbeddedComprehension <$> arbitrary
    ]
  shrink = \case
    EmbeddedExpression    e -> EmbeddedExpression    <$> shrink e
    EmbeddedComprehension e -> EmbeddedComprehension <$> shrink e

instance Arbitrary LetClause where
  arbitrary = LetClause
    <$> arbitrary
    <*> arbitrary
  shrink (LetClause i e) = LetClause i <$> shrink e

instance Arbitrary AliasedExpression where
  arbitrary = AliasedExpression
    <$> arbitrary
    <*> arbitrary
  shrink (AliasedExpression a e) = AliasedExpression a <$> shrink e


--------------------------------------------------------------------------------
-- Comprehension

instance Arbitrary Comprehension where
  arbitrary = Comprehension
    <$> ((:|) <$> firstFor <*> arbitrary)
    <*> arbitrary
    where
      firstFor = ComprehensionFor <$> arbitrary <*> arbitrary
  shrink (Comprehension (f :| clauses) decls) = concat
    [ Comprehension <$> ((:|) <$> shrink f <*> pure   clauses) <*> pure   decls
    , Comprehension <$> ((:|) <$> pure   f <*> shrink clauses) <*> pure   decls
    , Comprehension <$> ((:|) <$> pure   f <*> pure   clauses) <*> shrink decls
    ]

instance Arbitrary ComprehensionClause where
  arbitrary = oneof
    [ ComprehensionFor        <$> arbitrary               <*> arbitrary
    , ComprehensionIndexedFor <$> arbitrary <*> arbitrary <*> arbitrary
    , ComprehensionIf                                     <$> arbitrary
    , ComprehensionLet        <$> arbitrary               <*> arbitrary
    ]
  shrink = \case
    ComprehensionFor        i   e -> ComprehensionFor        i   <$> shrink e
    ComprehensionIndexedFor i j e -> ComprehensionIndexedFor i j <$> shrink e
    ComprehensionIf             e -> ComprehensionIf             <$> shrink e
    ComprehensionLet        i   e -> ComprehensionLet        i   <$> shrink e


--------------------------------------------------------------------------------
-- Expressions

instance Arbitrary Expression where
  arbitrary = sized \n -> e ops (min 3 n)
    where
      ops =
        [ -- precedence 1
          Disjunction
        , -- precedence 2
          Unification
        , -- precedence 3
          LogicalOr
        , -- precedence 4
          LogicalAnd
        , -- precedence 5
          Equal
        , NotEqual
        , Match
        , NotMatch
        , LessOrEqual
        , LessThan
        , GreaterOrEqual
        , GreaterThan
        , -- precedence 6
          Addition
        , Subtraction
        , -- precedence 7
          Multiplication
        , Division
        ]
      e _  0 = Unary <$> arbitrary
      e [] _ = Unary <$> arbitrary
      e l  d = do
        i <- chooseInt (0, length l)
        case drop i l of
          []    -> Unary <$> arbitrary
          (o:z) -> do
            el <- e z (d-1)
            er <- e z (d-1)
            pure $ o el er
  shrink = \case
    Unary ue           -> Unary <$> shrink ue
    Multiplication l r -> sh Multiplication l r
    Division       l r -> sh Division       l r
    Addition       l r -> sh Addition       l r
    Subtraction    l r -> sh Subtraction    l r
    Equal          l r -> sh Equal          l r
    NotEqual       l r -> sh NotEqual       l r
    Match          l r -> sh Match          l r
    NotMatch       l r -> sh NotMatch       l r
    LessThan       l r -> sh LessThan       l r
    LessOrEqual    l r -> sh LessOrEqual    l r
    GreaterThan    l r -> sh GreaterThan    l r
    GreaterOrEqual l r -> sh GreaterOrEqual l r
    LogicalAnd     l r -> sh LogicalAnd     l r
    LogicalOr      l r -> sh LogicalOr      l r
    Unification    l r -> sh Unification    l r
    Disjunction    l r -> sh Disjunction    l r
    where
      sh c l r =
        -- sub-expressions
        [l, r] <> concat
        -- same operator, simplified sub-expression
        [ c <$> shrink l <*> pure   r
        , c <$> pure   l <*> shrink r
        ]

instance Arbitrary UnaryExpression where
  arbitrary = UnaryExpression
    <$> listOf arbitraryUnaryOperator
    <*> arbitrary
  shrink (UnaryExpression ops e) = concat
    [ UnaryExpression <$> shrinkOps ops <*> pure   e
    , UnaryExpression <$> pure      ops <*> shrink e
    ]
    where
      shrinkOps = shrinkList shrinkNothing

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
  shrink = \case
    PrimaryOperand  o -> PrimaryOperand <$> shrink o
    PrimaryIndex pe x -> sh PrimaryIndex pe x
    PrimarySlice pe x -> sh PrimarySlice pe x
    PrimaryCall  pe x -> sh PrimaryCall  pe x
    _                 -> unreachable
    where
      sh c pe x =
        -- remove the suffix
        pe : concat
        -- keep it but simplify its components
        [ c <$> shrink pe <*> pure   x
        , c <$> pure   pe <*> shrink x
        ]

instance Arbitrary Operand where
  arbitrary = sized o
    where
      o n = frequency
        [ (2,       OperandLiteral    <$> scale (`div` 2) arbitrary)
        , (2,       OperandName       <$> scale (`div` 2) arbitrary)
        , (min 1 n, OperandExpression <$> scale (`div` 5) arbitrary)
        ]
  shrink = \case
    OperandLiteral    l -> OperandLiteral    <$> shrink l
    OperandName       n -> OperandName       <$> shrink n
    OperandExpression e -> OperandExpression <$> shrink e


--------------------------------------------------------------------------------
-- Literals

instance Arbitrary Literal where
  arbitrary = sized \s -> oneof $ concat
    [ [ IntegerLiteral . getPositive <$> arbitrary
      , FloatLiteral   . getPositive <$> arbitrary
      , BoolLiteral                  <$> arbitrary
      , pure NullLiteral
      , pure BottomLiteral
      ]
    , guard (s > 0) *>
      [ StringLiteral <$> scale (`div` 5) arbitraryStringLiteral
      , StructLiteral <$> scale (`div` 5) arbitrary
      , ListLiteral   <$> scale (`div` 5) arbitrary
      ]
    ]
  shrink = \case
    StringLiteral sl ->
      StringLiteral <$> shrinkStringLiteral sl
    StructLiteral s ->
      StructLiteral <$> shrink s
    ListLiteral l ->
      ListLiteral   <$> shrink l
    _ -> []

instance Arbitrary ListLiteral where
  arbitrary = oneof
    [ ClosedList <$> arbitrary
    , OpenList   <$> arbitrary <*> arbitrary
    ]
  shrink = \case
    ClosedList elems          -> ClosedList <$> shrink elems
    OpenList   elems ellipsis ->
      -- remove ellipsis
      ClosedList elems : concat
      -- shrink components
      [ OpenList <$> shrink elems <*> pure   ellipsis
      , OpenList <$> pure   elems <*> shrink ellipsis
      ]


--------------------------------------------------------------------------------
-- Helpers

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

arbitraryText1 :: Gen Text
arbitraryText1 = T.pack <$> listOf1 arbitrary

arbitraryImport :: Gen Text
arbitraryImport = T.pack <$> listOf1 (elements ['a'..'z'])

arbitraryNonEmpty :: Arbitrary a => Gen (NonEmpty a)
arbitraryNonEmpty = NE.fromList <$> listOf1 arbitrary

arbitraryStringLiteral :: Gen StringLiteral
arbitraryStringLiteral = listOf $ oneof
  [ Interpolation <$> arbitrary
  , RawStringLiteral "\"" <$> arbitraryText
  ]

shrinkStringLiteral :: StringLiteral -> [StringLiteral]
shrinkStringLiteral = shrinkList \case
  Interpolation e      -> Interpolation    <$> shrink e
  RawStringLiteral _ _ -> []

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
