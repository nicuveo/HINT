{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitrary where

import           Control.Monad
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Test.Tasty.QuickCheck

import           Lang.Cue.Grammar


--------------------------------------------------------------------------------
-- Tokens

instance Arbitrary Token where
  arbitrary = sized \s -> frequency $
    let
      a = min 1 s
    in
      [ (1, TokenInteger . getPositive <$> arbitrary)
      , (1, TokenFloat   . getPositive <$> arbitrary)
      , (1, TokenIdentifier    <$> arbitrary)
      , (1, TokenKeyword       <$> arbitrary)
      , (1, TokenOperator      <$> arbitraryTokenOperator)
      , (1, TokenString        <$> arbitraryText)
      , (a, TokenAttribute     <$> arbitrary)
      ]
  shrink = \case
    TokenAttribute (Attribute name attrTokens) -> do
      toks <- shrink attrTokens
      pure $ TokenAttribute $ Attribute name toks
    _ -> []

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

instance Arbitrary Attribute where
  arbitrary = Attribute <$> arbitrary <*> arbitrary

instance Arbitrary AttributeToken where
  arbitrary = sized \s -> frequency
    [ (6,       AttributeToken    <$> scale (`div` 2) arbitrary)
    , (min 1 s, AttributeParens   <$> scale (`div` 5) arbitrary)
    , (min 1 s, AttributeBraces   <$> scale (`div` 5) arbitrary)
    , (min 1 s, AttributeBrackets <$> scale (`div` 5) arbitrary)
    ]
  shrink = \case
    AttributeToken    t  -> []
    AttributeParens   ts -> AttributeParens   <$> shrink ts
    AttributeBraces   ts -> AttributeBraces   <$> shrink ts
    AttributeBrackets ts -> AttributeBrackets <$> shrink ts


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
    <*> arbitrary

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
    [ LabelString     <$> arbitraryStringLiteral <*> arbitrary
    , LabelIdentifier <$> arbitrary <*> arbitrary
    , LabelConstraint <$> arbitrary
    ]
  shrink = \case
    LabelIdentifier _ _ -> []
    LabelString     t o -> LabelString <$> shrinkStringLiteral t <*> pure o
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
          (o:r) -> let x = e r (d-1) in o <$> x <*> x <*> listOf x
  shrink = \case
    Unary ue              -> Unary <$> shrink ue
    Multiplication l r vs -> sh Multiplication l r vs
    Division       l r vs -> sh Division       l r vs
    Addition       l r vs -> sh Addition       l r vs
    Subtraction    l r vs -> sh Subtraction    l r vs
    Equal          l r vs -> sh Equal          l r vs
    NotEqual       l r vs -> sh NotEqual       l r vs
    Match          l r vs -> sh Match          l r vs
    NotMatch       l r vs -> sh NotMatch       l r vs
    LessThan       l r vs -> sh LessThan       l r vs
    LessOrEqual    l r vs -> sh LessOrEqual    l r vs
    GreaterThan    l r vs -> sh GreaterThan    l r vs
    GreaterOrEqual l r vs -> sh GreaterOrEqual l r vs
    LogicalAnd     l r vs -> sh LogicalAnd     l r vs
    LogicalOr      l r vs -> sh LogicalOr      l r vs
    Unification    l r vs -> sh Unification    l r vs
    Disjunction    l r vs -> sh Disjunction    l r vs
    where
      sh c l r vs =
        -- sub-expressions
        l : r : vs <> concat
        -- same operator, simplified sub-expression
        [ c <$> shrink l <*> pure   r <*> pure   vs
        , c <$> pure   l <*> shrink r <*> pure   vs
        , c <$> pure   l <*> pure   r <*> shrink vs
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
        , (min 1 n, OperandExpression <$> scale (`div` 2) arbitrary)
        ]
  shrink = \case
    OperandLiteral    l -> OperandLiteral    <$> shrink l
    OperandName       n -> OperandName       <$> shrink n
    OperandExpression e -> OperandExpression <$> shrink e

instance Arbitrary QualifiedIdentifier where
  arbitrary = QualifiedIdentifier
    <$> arbitrary
    <*> arbitrary

--------------------------------------------------------------------------------
-- Literals

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
      [ StringLiteral . Left  <$> scale (`div` 5) arbitraryInterpolation
      , ListLiteral           <$> scale (`div` 5) arbitrary
      , StructLiteral         <$> scale (`div` 5) arbitrary
      ]
    ]
  shrink = \case
    StringLiteral (Left interpolation) ->
      StringLiteral . Left <$> shrinkInterpolation interpolation
    ListLiteral l ->
      ListLiteral <$> shrink l
    StructLiteral s ->
      StructLiteral <$> shrink s
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
arbitraryStringLiteral = oneof
  [ Left  <$> arbitraryInterpolation
  , Right <$> arbitraryText
  ]

shrinkStringLiteral :: StringLiteral -> [StringLiteral]
shrinkStringLiteral = \case
  Left  i -> Left <$> shrinkInterpolation i
  Right _ -> []

arbitraryInterpolation :: Gen Interpolation
arbitraryInterpolation = do
  a <- arbitraryText1
  b <- arbitrary
  c <- arbitraryText1
  pure [InterpolationString a, InterpolationExpression b, InterpolationString c]

shrinkInterpolation :: Interpolation -> [Interpolation]
shrinkInterpolation = \case
  [InterpolationString a, InterpolationExpression b, InterpolationString c] -> do
    b' <- shrink b
    pure [InterpolationString a, InterpolationExpression b', InterpolationString c]
  _ -> error "invalid interpolation"

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
