{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RecursiveDo #-}

module Lang.Cue.Parser (parse, sourceFile, expression) where

import "this" Prelude

import Control.Applicative.Combinators
import Text.Earley hiding (Parser, Grammar, rule)
import Text.Earley qualified as E

import Lang.Cue.AST
import Lang.Cue.Error
import Lang.Cue.Tokens
import Lang.Cue.Lexer
import Lang.Cue.Location


--------------------------------------------------------------------------------
-- * Parser

type Grammar r a = E.Grammar r (Parser r a)
type Parser r = Prod r Text (Token WithOffset)

parse
  :: (forall r. Grammar r a)
  -> String
  -> Text
  -> Either ParseError a
parse grammar filename code = do
  tokens <- tokenize filename code
  case E.fullParses (E.parser grammar) tokens of
    ([], _report) -> Left undefined
    ([result], _)    -> Right result
    _                -> panic AmbiguousParse


--------------------------------------------------------------------------------
-- * Grammars

sourceFile :: Grammar r SourceFile
sourceFile = fst <$> fullGrammar

expression :: Grammar r Expression
expression = snd <$> fullGrammar

fullGrammar :: forall r. E.Grammar r (Parser r SourceFile, Parser r Expression)
fullGrammar = mdo
  -- module structure

  source <- rule "source file" do
    a <- many     $ attribute     <* comma
    p <- optional $ packageClause <* comma
    i <- many     $ importDecl    <* comma
    d <- many     $ declaration   <* comma
    pure $ SourceFile p a (concat i) d

  packageClause <- rule "package clause" $
    keyword KeywordPackage *> packageName

  packageName <- rule "package name"
    identifier

  importDecl <- rule "import declaration" $
    keyword KeywordImport *> choice
      [ pure <$> importSpec
      , parens $ commaList importSpec
      ]

  importSpec <- rule "import spec" do
    name <- optional packageName
    path <- importPath
    pure $ Import name path

  importPath <- rule "import path"
    simpleStringLiteral

  -- declarations

  declaration <- rule "declaration" $ choice
    [ DeclarationEllipsis  <$> ellipsis
    , DeclarationLetClause <$> letClause
    , DeclarationAttribute <$> attribute
    , DeclarationField     <$> field
    , DeclarationEmbedding <$> embedding
    ]

  ellipsis <- rule "ellipsis" $
    operator OperatorEllipsis *> optional expression

  letClause <- rule "let clause" do
    keyword KeywordLet
    n <- identifier
    operator OperatorBind
    e <- expression
    pure $ LetClause n e

  field <- rule "field" do
    labels  <- some $ label <* operator OperatorColon
    fExpr   <- aliasedExpression
    attribs <- many attribute
    pure $ (mkField fExpr labels) { fieldAttributes = attribs }

  label <- rule "label" do
    lname <- optional $ identifier <* operator OperatorBind
    lexpr <- labelExpr
    pure $ Label lname lexpr

  labelExpr <- rule "label expression" $
    identifierLabel <|> stringLabel <|> constraintLabel

  identifierLabel <- inlineRule do
    lname <- named "label name" identifier
    opt   <- optionality
    pure $ LabelIdentifier lname opt

  stringLabel <- inlineRule do
    lname <- named "label name" simpleStringLiteral
    opt   <- optionality
    pure $ LabelString lname opt

  constraintLabel <- inlineRule do
    LabelConstraint <$> brackets aliasedExpression

  optionality <- inlineRule $
    optional (operator OperatorOption) <&> \case
      Just _  -> Optional
      Nothing -> Required

  embedding <- rule "embedding" $ choice
    [ EmbeddedComprehension <$> comprehension
    , EmbeddedExpression    <$> aliasedExpression
    ]

  -- expression

  let level term ops = binary term ops <|> term
      binary term ops = choice $
        ops <&> \(op, cons) -> do
          -- left is at same precedence (left recursion)
          l <- level term ops
          operator op
          -- right is next precedence
          r <- term
          pure $ cons l r

  aliasedExpression <- rule "aliased expression" do
    alias <- optional $ identifier <* operator OperatorBind
    aexpr <- expression
    pure $ AliasedExpression alias aexpr

  expression <- rule "expression" precedence1

  precedence1 <- inlineRule $ level precedence2
    [ (OperatorOr, Disjunction)
    ]
  precedence2 <- inlineRule $ level precedence3
    [ (OperatorAnd, Unification)
    ]
  precedence3 <- inlineRule $ level precedence4
    [ (OperatorLOr, LogicalOr)
    ]
  precedence4 <- inlineRule $ level precedence5
    [ (OperatorLAnd, LogicalAnd)
    ]
  precedence5 <- inlineRule $ level precedence6
    [ (OperatorEqual,    Equal)
    , (OperatorNotEqual, NotEqual)
    , (OperatorMatch,    Match)
    , (OperatorNotMatch, NotMatch)
    , (OperatorLTE,      LessOrEqual)
    , (OperatorLT,       LessThan)
    , (OperatorGTE,      GreaterOrEqual)
    , (OperatorGT,       GreaterThan)
    ]
  precedence6 <- inlineRule $ level precedence7
    [ (OperatorAdd, Addition)
    , (OperatorSub, Subtraction)
    ]
  precedence7 <- inlineRule $ level unary
    [ (OperatorMul, Multiplication)
    , (OperatorQuo, Division)
    ]

  unary <- rule "unary expression" do
    ops <- many $ operators
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
    pe <- primaryExpression
    pure $ Unary $ UnaryExpression ops pe

  primaryExpression <- rule "primary expression" $
    primSelector <|> primIndex <|> primSlice <|> primCall <|> fmap PrimaryOperand operand

  primSelector <- inlineRule do
    prim <- primaryExpression
    sel  <- selector
    pure $ PrimarySelector prim sel

  primIndex <- inlineRule do
    prim <- primaryExpression
    ind  <- index
    pure $ PrimaryIndex prim ind

  primSlice <- inlineRule do
    prim <- primaryExpression
    sli  <- slice
    pure $ PrimarySlice prim sli

  primCall <- inlineRule do
    prim <- primaryExpression
    args <- arguments
    pure $ PrimaryCall prim args

  selector <- rule "selector" $
    operator OperatorPeriod *> choice
      [ Left  <$> identifier
      , Right <$> simpleStringLiteral
      ]

  index <- rule "index" $
    brackets expression

  slice <- rule "slice" $ brackets do
    low  <- expression
    operator OperatorColon
    high <- expression
    pure (low, high)

  arguments <- rule "arguments" $
    parens $ commaList argument

  argument <- rule "argument" expression

  operand <- rule "operand" $ choice
    [ OperandLiteral    <$> literal
    , OperandName       <$> operandName
    , OperandExpression <$> parens expression
    ]

  operandName <- rule "operand name" $ choice
    [ QualifiedIdentifier Nothing <$> identifier
    , qualifiedIdentifier
    ]

  qualifiedIdentifier <- rule "qualified identifier" do
    pn <- packageName
    operator OperatorPeriod
    ident <- identifier
    pure $ QualifiedIdentifier (Just pn) ident

  -- literal

  literal <- rule "literal" $
    basicLiteral <|> fmap ListLiteral listLiteral <|> fmap StructLiteral structLiteral

  basicLiteral <- rule "basic literal" $ choice
    [ IntegerLiteral    <$> integerLiteral
    , FloatLiteral      <$> floatLiteral
    , StringLiteral     <$> (stringLiteral expression)
    , BoolLiteral True  <$  keyword  KeywordTrue
    , BoolLiteral False <$  keyword  KeywordFalse
    , NullLiteral       <$  keyword  KeywordNull
    , BottomLiteral     <$  operator OperatorBottom
    ]

  listLiteral <- rule "list literal" $
    brackets $ fmap (fromMaybe $ ClosedList []) $ optional $ listBody <* optional comma

  listBody <- rule "list body" $ choice
    [ OpenList [] <$> ellipsis
    , listInternal
    ]

  listInternal <- inlineRule do
    es <- embedding `sepBy1` comma
    el <- optional $ comma *> ellipsis
    pure $ case el of
      Nothing -> ClosedList es
      Just e  -> OpenList   es e

  structLiteral <- rule "struct literal" $
    braces $ (declaration `sepBy` comma) <* optional comma

  -- comprehension

  comprehension <- rule "comprehension" $
    liftA2 Comprehension clauses structLiteral

  clauses <- rule "clauses" $
    liftA2 (:|) startClause (many $ optional comma *> clause)

  startClause <- rule "start clause" $
    forClause <|> guardClause

  clause <- rule "clause" $
    startClause <|> fmap toComp letClause

  forClause <- rule "for clause" do
    keyword KeywordFor
    ident1 <- identifier
    ident2 <- optional $ comma *> identifier
    keyword KeywordIn
    expr <- expression
    pure $ case ident2 of
      Just i  -> ComprehensionIndexedFor ident1 i expr
      Nothing -> ComprehensionFor        ident1   expr

  guardClause <- rule "guard clause" $
    keyword KeywordIf *> fmap ComprehensionIf expression

  -- return all possible entry points

  pure (source, expression)


--------------------------------------------------------------------------------
-- * Terminals

identifier :: Parser r Identifier
identifier = named "identifier" $ terminal \case
  TokenIdentifier (discardOffset -> a) -> Just a
  _ -> Nothing

keyword :: Keyword -> Parser r ()
keyword kw = named name $ terminal \case
  TokenKeyword (discardOffset -> k) | kw == k -> Just ()
  _ -> Nothing
  where
    name = case kw of
      KeywordPackage -> "\"package\" keyword"
      KeywordImport  -> "\"import\" keyword"
      KeywordNull    -> "\"null\" keyword"
      KeywordTrue    -> "\"true\" keyword"
      KeywordFalse   -> "\"false\" keyword"
      KeywordFor     -> "\"for\" keyword"
      KeywordIn      -> "\"in\" keyword"
      KeywordIf      -> "\"if\" keyword"
      KeywordLet     -> "\"let\" keyword"

operator :: Operator -> Parser r Operator
operator op = named name $ terminal \case
  TokenOperator (discardOffset -> o) | op == o -> Just op
  _ -> Nothing
  where
    name = case op of
      OperatorRealComma     -> "comma"
      OperatorNewlineComma  -> "comma"
      OperatorEOFComma      -> "comma"
      OperatorAdd           -> "+"
      OperatorSub           -> "-"
      OperatorMul           -> "*"
      OperatorPow           -> "^"
      OperatorQuo           -> "/"
      OperatorArrow         -> "<-"
      OperatorLAnd          -> "&&"
      OperatorLOr           -> "||"
      OperatorAnd           -> "&"
      OperatorOr            -> "|"
      OperatorEqual         -> "=="
      OperatorNotEqual      -> "!="
      OperatorMatch         -> "=~"
      OperatorNotMatch      -> "!~"
      OperatorLTE           -> "<="
      OperatorGTE           -> ">="
      OperatorLT            -> "<"
      OperatorGT            -> ">"
      OperatorBind          -> "="
      OperatorIsA           -> "::"
      OperatorColon         -> ":"
      OperatorOption        -> "?"
      OperatorNot           -> "!"
      OperatorEllipsis      -> "..."
      OperatorPeriod        -> "."
      OperatorBottom        -> "_|_"
      OperatorParensOpen    -> "("
      OperatorParensClose   -> ")"
      OperatorBracesOpen    -> "{"
      OperatorBracesClose   -> "}"
      OperatorBracketsOpen  -> "["
      OperatorBracketsClose -> "]"

operators :: [Operator] -> Parser r Operator
operators = choice . map operator

attribute :: Parser r Attribute
attribute = named "attribute" $ terminal \case
  TokenAttribute (discardOffset -> a) -> Just a
  _ -> Nothing

simpleStringLiteral :: Parser r Text
simpleStringLiteral = named "simple string literal" $ terminal \case
  TokenString (discardOffset -> (d, s)) | d == "\"" -> Just s
  _ -> Nothing

stringLiteral :: Parser r Expression -> Parser r StringLiteral
stringLiteral expr = named "string literal" $ fmap pure rawLiteral <|> interpolation
  where
    rawLiteral = named "raw string literal" $ terminal \case
      TokenString (discardOffset -> (d, s)) -> Just (RawStringLiteral d s)
      _ -> Nothing
    interpolation = named "interpolation" do
      terminal \case
        TokenInterpolationBegin _ -> Just ()
        _ -> Nothing
      elts <- many $ rawLiteral <|> interpExpr
      terminal \case
        TokenInterpolationBegin _ -> Just ()
        _ -> Nothing
      pure elts
    interpExpr = named "interpolation expression" do
      terminal \case
        TokenInterpolationExprBegin _ -> Just ()
        _ -> Nothing
      e <- Interpolation <$> expr
      terminal \case
        TokenInterpolationExprBegin _ -> Just ()
        _ -> Nothing
      pure e

integerLiteral :: Parser r Integer
integerLiteral = named "integer literal" $ terminal \case
  TokenInteger (discardOffset -> i) -> Just i
  _ -> Nothing

floatLiteral :: Parser r Double
floatLiteral = named "float literal" $ terminal \case
  TokenFloat (discardOffset -> d) -> Just d
  _ -> Nothing


--------------------------------------------------------------------------------
-- * Combinators

-- | Expects a comma, regardless of whether it was explicit or added by a newline.
comma :: Parser r ()
comma = named "comma or newline" $ void $ choice
  [ operator OperatorRealComma
  , operator OperatorNewlineComma
  , operator OperatorEOFComma
  ]

-- | Expects an explicit comma, for lists.
explicitComma :: Parser r ()
explicitComma = named "comma" $ void $ operator OperatorRealComma

parens :: Parser r a -> Parser r a
parens = between (operator OperatorParensOpen) (operator OperatorParensClose)

braces :: Parser r a -> Parser r a
braces = between (operator OperatorBracesOpen) (operator OperatorBracesClose)

brackets :: Parser r a -> Parser r a
brackets = between (operator OperatorBracketsOpen) (operator OperatorBracketsClose)

commaList :: Parser r a -> Parser r [a]
commaList p = (p `sepBy` explicitComma) <* optional explicitComma


--------------------------------------------------------------------------------
-- * Tools

named :: Text -> Parser r a -> Parser r a
named = flip (<?>)

rule :: Text -> Parser r a -> Grammar r a
rule = inlineRule ... named

inlineRule :: Parser r a -> Grammar r a
inlineRule = E.rule


--------------------------------------------------------------------------------
-- * AST helpers

-- | Rewrites @foo:bar:baz: 42@ into @foo: { bar: { baz: 42 }}@
mkField :: AliasedExpression -> [Label] -> Field
mkField expr = \case
  []       -> unreachable
  [l]      -> Field l expr []
  (l:subs) -> Field l (buildExpr $ mkField expr subs) []
  where
    buildExpr = AliasedExpression Nothing
      . Unary
      . UnaryExpression []
      . PrimaryOperand
      . OperandLiteral
      . StructLiteral
      . pure
      . DeclarationField

-- | Converts a 'LetClause' into a let 'ComprehensionClause'
toComp :: LetClause -> ComprehensionClause
toComp (LetClause ident expr) = ComprehensionLet ident expr


{-

importSpec :: Parser Import
importSpec = do
  name <- optional packageName
  (path, ident) <- impPath
  pure $ Import name path ident
  where
    illegal = "!\"#$%&'()*,:;<=>?[]^{|}\xFFFD" :: String
    isValid c = isPrint c && not (isSpace c) && notElem c illegal
    impPath = do
      path <- simpleStringLiteral 0 >>= \case
        Left  _ -> fail "interpolations not allowed in import paths"
        Right t -> pure t
      let
        (filePath, suffix) = case T.breakOnEnd ":" path of
          (part1, part2)
            | T.null part1 -> (part2, Nothing)
            | otherwise    -> (T.dropEnd 1 part1, Just part2)
      unless (T.all isValid filePath) $
        fail $ "import path contains invalid characters"
      alias <- traverse mkIdentifier suffix
      pure (filePath, alias)

-}
