{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RecursiveDo #-}

module Lang.Cue.Parser where

import "this" Prelude

import Control.Applicative.Combinators
import Text.Earley hiding (Parser, Grammar, rule)
import Text.Earley qualified as E

import Lang.Cue.AST
import Lang.Cue.Error
import Lang.Cue.Tokens
import Lang.Cue.Location


--------------------------------------------------------------------------------
-- * Parser

type Grammar r a = E.Grammar r (Parser r a)
type Parser r = Prod r Text (Token WithOffset)

parse
  :: Grammar r a
  -> String
  -> Text
  -> Either ParseError a
parse = undefined


--------------------------------------------------------------------------------
-- * Grammars

sourceFile :: Grammar r SourceFile
sourceFile = mdo
  res <- rule "source file" do
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

  importPath <- rule "import path" $ terminal \case
    TokenString (discardOffset -> (d, s)) | d == "\"" -> Just s
    _ -> Nothing

  pure res



{-

earley cannot do validation; move this elsewhere

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

operator :: Operator -> Parser r ()
operator op = named name $ terminal \case
  TokenOperator (discardOffset -> o) | op == o -> Just ()
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

attribute :: Parser r Attribute
attribute = named "attribute" $ terminal \case
  TokenAttribute (discardOffset -> a) -> Just a
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
comma = named "comma or newline" $ choice
  [ operator OperatorRealComma
  , operator OperatorNewlineComma
  , operator OperatorEOFComma
  ]

-- | Expects an explicit comma, for lists.
explicitComma :: Parser r ()
explicitComma = named "comma" $ operator OperatorRealComma

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
rule = E.rule ... named


declaration = undefined

{-

-}


--------------------------------------------------------------------------------
-- * Grammar

expression = undefined


{-


import "this" Prelude             hiding (exponent)

import Data.Char
import Data.List                  qualified as L
import Data.List.NonEmpty         (NonEmpty (..))
import Data.Text                  qualified as T
import Text.Megaparsec            hiding (Label, Token, token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Lang.Cue.Error
import Lang.Cue.Grammar

--------------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void Text

-- | Expects a comma, regardless of whether it was explicit or added by a newline.
comma :: Parser Operator
comma = label "comma or newline" $ operators [OperatorRealComma, OperatorNewlineComma, OperatorEOFComma]

-- | Expects an explicit comma, for lists.
explicitComma :: Parser Operator
explicitComma = label "comma" $ operator OperatorRealComma

parens :: Parser a -> Parser a
parens = between (operator OperatorParensOpen) (operator OperatorParensClose)

braces :: Parser a -> Parser a
braces = between (operator OperatorBracesOpen) (operator OperatorBracesClose)

brackets :: Parser a -> Parser a
brackets = between (operator OperatorBracketsOpen) (operator OperatorBracketsClose)

commaList :: Parser a -> Parser [a]
commaList p = (p `sepBy` explicitComma) <* optional explicitComma


--------------------------------------------------------------------------------
-- File structure

sourceFile :: Parser SourceFile
sourceFile = do
  skipToNextToken False
  a <- many $ attribute <* comma
  p <- optional $ packageClause <* comma
  i <- many $ importDecl <* comma
  d <- block
  eof
  pure $ SourceFile p a (concat i) d

packageClause :: Parser Identifier
packageClause = keyword KeywordPackage >> packageName

packageName :: Parser Identifier
packageName = identifier

importDecl :: Parser [Import]
importDecl = do
  keyword KeywordImport
  choice
    [ pure <$> importSpec
    , parens $ commaList importSpec
    ]

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


--------------------------------------------------------------------------------
-- Declarations

block :: Parser [Declaration]
block = many $ declaration <* comma

declaration :: Parser Declaration
declaration = choice
  [ DeclarationEllipsis  <$> ellipsis
  , DeclarationLetClause <$> letClause
  , DeclarationAttribute <$> attribute
  , DeclarationField     <$> field
  , DeclarationEmbedding <$> embedding
  , fail "expected a declaration: one of [ellipsis, let clause, attribute, field or embedding]"
  ]

ellipsis :: Parser Ellipsis
ellipsis = operator OperatorEllipsis *> optional expression

letClause :: Parser LetClause
letClause = do
  keyword KeywordLet
  n <- identifier
  operator OperatorBind
  e <- expression
  pure $ LetClause n e

field :: Parser Field
field = do
  labels  <- some $ try $ fieldLabel <* operator OperatorColon
  fexpr   <- aliasedExpression
  attribs <- many attribute
  let result = go fexpr labels
  pure $ result { fieldAttributes = attribs }
  where
    mkExpr = AliasedExpression Nothing
      . Unary
      . UnaryExpression []
      . PrimaryOperand
      . OperandLiteral
      . StructLiteral
      . pure
      . DeclarationField
    go _    []       = unreachable
    go expr [l]      = Field l expr []
    go expr (l:subs) = Field l (mkExpr $ go expr subs) []

fieldLabel :: Parser Label
fieldLabel = do
  name <- optional $ try $ identifier <* operator OperatorBind
  expr <- labelExpr
  pure $ Label name expr

labelExpr :: Parser LabelExpression
labelExpr = stringLabel <|> constraintLabel <|> identifierLabel
  where
    optionality = optional (operator OperatorOption) <&> \case
      Just _  -> Optional
      Nothing -> Required
    stringLabel = do
      text <- simpleStringLiteral 0
      opt  <- optionality
      pure $ LabelString text opt
    identifierLabel = do
      ident <- identifier
      opt   <- optionality
      pure $ LabelIdentifier ident opt
    constraintLabel =
      LabelConstraint <$> brackets aliasedExpression

embedding :: Parser Embedding
embedding = choice
  [ EmbeddedComprehension <$> comprehension
  , EmbeddedExpression    <$> aliasedExpression
  ]


--------------------------------------------------------------------------------
-- Expressions

expression :: Parser Expression
expression = binaryOp binaryOperators
  where
    binaryOp [] = Unary <$> unaryExpression
    binaryOp ((op, constructor):rest) = do
      lhs <- binaryOp rest
      rhs <- many $ operator op *> binaryOp rest
      pure $ case rhs of
        []    -> lhs
        (r:v) -> constructor lhs r v
    binaryOperators =
      [ -- precedence 1
        (OperatorOr, Disjunction)
      , -- precedence 2
        (OperatorAnd, Unification)
      , -- precedence 3
        (OperatorLOr, LogicalOr)
      , -- precedence 4
        (OperatorLAnd, LogicalAnd)
      , -- precedence 5
        (OperatorEqual,    Equal)
      , (OperatorNotEqual, NotEqual)
      , (OperatorMatch,    Match)
      , (OperatorNotMatch, NotMatch)
      , (OperatorLTE,      LessOrEqual)
      , (OperatorLT,       LessThan)
      , (OperatorGTE,      GreaterOrEqual)
      , (OperatorGT,       GreaterThan)
      , -- precedence 6
        (OperatorAdd, Addition)
      , (OperatorSub, Subtraction)
      , -- precedence 7
        (OperatorMul, Multiplication)
      , (OperatorQuo, Division)
      ]

aliasedExpression :: Parser AliasedExpression
aliasedExpression = do
  alias <- optional $ try $ identifier <* operator OperatorBind
  expr  <- expression
  pure $ AliasedExpression alias expr

unaryExpression :: Parser UnaryExpression
unaryExpression = do
  ops <- many $ operators unaryOperators
  pe  <- primaryExpression
  pure $ UnaryExpression ops pe
  where
    unaryOperators =
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

primaryExpression :: Parser PrimaryExpression
primaryExpression = do
  o <- PrimaryOperand <$> operand
  suffixes o
  where
    suffixes pexp = option pexp $ choice
      [ call pexp
      , selector pexp
      , indexOrSlice pexp
      ]
    call pexp = do
      operator OperatorParensOpen
      res <- optional (operator OperatorParensClose) >>= \case
        Just _  -> pure $ PrimaryCall pexp []
        Nothing -> do
          h <- expression
          t <- (explicitComma *> expression) `manyTill`
            try (optional explicitComma *> operator OperatorParensClose)
          pure $ PrimaryCall pexp (h:t)
      suffixes res
    selector pexp = do
      operator OperatorPeriod
      sel <- choice
        [ Left <$> identifier
        , Right <$> simpleStringLiteral 0
        ]
      suffixes $ PrimarySelector pexp sel
    indexOrSlice pexp =
      suffixes =<< brackets do
        low <- expression
        sep <- optional $ operator OperatorColon
        case sep of
          Nothing ->
            pure $ PrimaryIndex pexp low
          Just _  -> do
            high <- expression
            pure $ PrimarySlice pexp (low, high)

-- | Parses a primary operand.
--
-- The order of operations here is tricky, because of some conflicting syntax:
--   - @_|_@ can parse as either the bottom literal, or as the disjunction of
--     the identifiers @_@ and @_@ (if allowed)
--   - # can be the start of an identifier OR of a string literal
--
-- We `try` the case of the identifier first, to avoid backtracking all the way
-- if something is wrong within a string literal, as it might itself contain an
-- expression due to interpolation, and we deal with bottom before anything
-- else.
operand :: Parser Operand
operand = choice
  [ bottom
  , OperandExpression <$> parens expression
  , OperandName       <$> qualifiedIdentifier
  , OperandLiteral    <$> literal
  ]
  where
    bottom = OperandLiteral BottomLiteral <$ operator OperatorBottom

qualifiedIdentifier :: Parser QualifiedIdentifier
qualifiedIdentifier = try $ do
  id1 <- identifier
  sep <- optional $ operator OperatorPeriod
  case sep of
    Nothing ->
      pure $ QualifiedIdentifier Nothing id1
    Just _ -> do
      id2 <- identifier
      pure $ QualifiedIdentifier (Just id1) id2


--------------------------------------------------------------------------------
-- Comprehension

comprehension :: Parser Comprehension
comprehension = Comprehension
  <$> clauses
  <*> structLiteral

clauses :: Parser (NonEmpty ComprehensionClause)
clauses = (:|)
  <$> startClause
  <*> many (optional comma *> clause)
  where
    clause = startClause <|> compLetClause
    startClause = forClause <|> guardClause
    compLetClause = do
      LetClause ident expr <- letClause
      pure $ ComprehensionLet ident expr

forClause :: Parser ComprehensionClause
forClause = do
  keyword KeywordFor
  ident1 <- identifier
  ident2 <- optional $ comma *> identifier
  keyword KeywordIn
  expr <- expression
  pure $ case ident2 of
    Just i  -> ComprehensionIndexedFor ident1 i expr
    Nothing -> ComprehensionFor        ident1   expr

guardClause :: Parser ComprehensionClause
guardClause = do
  keyword KeywordIf
  ComprehensionIf <$> expression


--------------------------------------------------------------------------------
-- Literal

literal :: Parser Literal
literal = choice
  [ either FloatLiteral IntegerLiteral <$> numberLiteral
  , StringLiteral     <$> stringLiteral
  , BoolLiteral True  <$  keyword  KeywordTrue
  , BoolLiteral False <$  keyword  KeywordFalse
  , NullLiteral       <$  keyword  KeywordNull
  , BottomLiteral     <$  operator OperatorBottom
  , StructLiteral     <$> structLiteral
  , ListLiteral       <$> listLiteral
  , fail "expecting a literal"
  ]

-- | Parses a number token, and skips subsequent space.
numberLiteral :: Parser (Either Double Integer)
numberLiteral = do
  res <- choice
    [ Right <$> binary
    , Right <$> octal
    , Right <$> hex
    , try num
    , Right <$> decimal
    ]
  skipToNextToken True
  pure res
  where
    binary = do
      string "0b"
      value <- some binDigitChar
      pure $ L.foldl' (\acc d ->  2*acc + fromIntegral (digitToInt d)) 0 value
    octal = do
      string "0o"
      value <- some octDigitChar
      pure $ L.foldl' (\acc d ->  8*acc + fromIntegral (digitToInt d)) 0 value
    hex = do
      string "0x" <|> string "0X"
      value <- some hexDigitChar
      pure $ L.foldl' (\acc d -> 16*acc + fromIntegral (digitToInt d)) 0 value
    num = do
      part1 <- optional decimals
      dot   <- optional $ char '.'
      part2 <- optional decimals
      mult  <- fmap Left multiplier <|> fmap Right (optional exponent)
      case (part1, dot, part2, mult) of
        -- multiplier found: si number
        (Just p1, Nothing, Nothing, Left m) -> pure $ Right $ round $ (read p1               :: Double) * m
        (Just p1, Just _,  Just p2, Left m) -> pure $ Right $ round $ (read (p1 ++ '.' : p2) :: Double) * m
        (Nothing, Just _,  Just p2, Left m) -> pure $ Right $ round $ (read ("0." <> p2)     :: Double) * m
        (_      , _,       _,       Left _) -> fail "broken si value"
        -- no multiplier: floating point
        (Just p1, Just _,       p2, Right me)       -> pure $ Left $ read $ p1 <> "." <> fromMaybe "0" p2 <> fromMaybe "" me
        (Just p1, Nothing, Nothing, Right (Just e)) -> pure $ Left $ read $ p1 <> e
        (Nothing, Just _,  Just p2, Right me)       -> pure $ Left $ read $ "0." <> p2 <> fromMaybe "" me
        (_      , _,       _,       Right _)        -> fail "broken floating point value"
    decimal = (0 <$ string "0") <|> do
      h <- oneOf @[] "123456789"
      t <- many $ optional (char '_') *> digitChar
      pure $ read (h:t)
    decimals = do
      h <- digitChar
      t <- many $ optional (char '_') *> digitChar
      pure $ h:t
    exponent = do
      e <- oneOf @[] "eE"
      s <- optional $ oneOf @[] "+-"
      d <- decimals
      pure $ e : maybe "" pure s ++ d
    multiplier = do
      r <- choice
        [ 1 <$ char 'K'
        , 2 <$ char 'M'
        , 3 <$ char 'G'
        , 4 <$ char 'T'
        , 5 <$ char 'P'
        ]
      i <- optional (char 'i') <&> \case
        Just _  -> 1024
        Nothing -> 1000
      pure $ i ** r

stringLiteral :: Parser (Either [InterpolationElement] Text)
stringLiteral = do
  hashCount <- length <$> many (char '#')
  res <- choice
    [ multilineStringLiteral hashCount
    , multilineBytesLiteral  hashCount
    , simpleStringLiteral    hashCount
    , simpleBytesLiteral     hashCount
    , fail "expecting a string or bytes literal"
    ]
  count hashCount (char '#') <|>
    fail "the number of closing # must match the number in the opening"
  skipToNextToken True
  pure res

simpleStringLiteral :: Int -> Parser (Either [InterpolationElement] Text)
simpleStringLiteral hashCount = do
  char '"'
  res <- charLiteral hashCount False False `manyTill` char '"'
  pure $ postProcess res

multilineStringLiteral :: Int -> Parser (Either [InterpolationElement] Text)
multilineStringLiteral hashCount = do
  string "\"\"\"\n"
  res <- charLiteral hashCount True False `manyTill` multilineClosing "\"\"\""
  pure $ postProcess res

simpleBytesLiteral :: Int -> Parser (Either [InterpolationElement] Text)
simpleBytesLiteral hashCount = do
  char '\''
  res <- charLiteral hashCount False True `manyTill` char '\''
  pure $ postProcess res

multilineBytesLiteral :: Int -> Parser (Either [InterpolationElement] Text)
multilineBytesLiteral hashCount = do
  string "'''\n"
  res <- charLiteral hashCount True True `manyTill` multilineClosing "'''"
  pure $ postProcess res

multilineClosing :: Text -> Parser ()
multilineClosing s = try $ void $ char '\n' >> hspace >> string s

charLiteral :: Int -> Bool -> Bool -> Parser InterpolationElement
charLiteral hashCount allowNewline isSingleQuotes = do
  c <- anySingle
  if | c == '\n' && not allowNewline -> fail "unterminated in single-line literal"
     | c == '\\' -> optional (count hashCount $ char '#') >>= \case
         Nothing -> do
           s <- many $ char '#'
           pure $ InterpolationString $ T.pack $ '\\' : s
         Just _  -> do
           e <- oneOf @[] "abfnrtv/\\'\"(uUx01234567" <|> fail "invalid escape character"
           case e of
             'a'  -> pure $ InterpolationString "\a"
             'b'  -> pure $ InterpolationString "\b"
             'f'  -> pure $ InterpolationString "\f"
             'n'  -> pure $ InterpolationString "\n"
             'r'  -> pure $ InterpolationString "\r"
             't'  -> pure $ InterpolationString "\t"
             'v'  -> pure $ InterpolationString "\v"
             '/'  -> pure $ InterpolationString "/"
             '\\' -> pure $ InterpolationString "\\"
             '\'' -> do
               unless isSingleQuotes $
                 fail "unexpected escaped single quote in string literal"
               pure $ InterpolationString "'"
             '"'  -> do
               when isSingleQuotes $
                 fail "unexpected escaped double quote in bytes literal"
               pure $ InterpolationString "\""
             'u'  -> do
               -- TODO: handle out of bounds hex values
               value <- count 4 hexDigitChar <|> fail "expecting 4 hexadecimal digits after \\u"
               pure $ InterpolationString $ T.singleton $ chr $ L.foldl' (\acc d -> 16*acc + digitToInt d) 0 value
             'U'  -> do
               -- TODO: handle out of bounds hex values
               value <- count 8 hexDigitChar <|> fail "expecting 8 hexadecimal digits after \\U"
               pure $ InterpolationString $ T.singleton $ chr $ L.foldl' (\acc d -> 16*acc + digitToInt d) 0 value
             'x'  -> do
               -- TODO: handle out of bounds hex values
               unless isSingleQuotes $
                 fail "unexpected hex value in string literal"
               value <- count 2 hexDigitChar <|> fail "expecting 2 hexadecimal digits after \\x"
               pure $ InterpolationString $ T.singleton $ chr $ L.foldl' (\acc d -> 16*acc + digitToInt d) 0 value
             '(' -> do
               -- TODO: what about newlines?
               skipToNextToken True
               expr <- expression
               char ')' <|> fail "expecting closing paren at the end of interpolation"
               pure $ InterpolationExpression expr
             oct -> do
               unless isSingleQuotes $
                 fail "unexpected octal value in string literal"
               value <- count 2 octDigitChar <|> fail "expecting 3 octal digits after \\"
               pure $ InterpolationString $ T.singleton $ chr $ L.foldl' (\acc d -> 8*acc + digitToInt d) 0 $ oct:value
     | otherwise -> pure $ InterpolationString $ T.singleton c

postProcess :: [InterpolationElement] -> Either [InterpolationElement] Text
postProcess l = case foldr fuse [] l of
  []                      -> Right ""
  [InterpolationString t] -> Right t
  heterogeneousList       -> Left heterogeneousList
  where
    fuse (InterpolationString t1) (InterpolationString t2 : r) = InterpolationString (t1 <> t2) : r
    fuse x r = x : r

structLiteral :: Parser [Declaration]
structLiteral = operator OperatorBracesOpen *> do
  optional (operator OperatorBracesClose) >>= \case
    Just _  -> pure []
    Nothing -> do
      h <- declaration
      t <- (comma *> declaration) `manyTill` (try $ optional comma >> operator OperatorBracesClose)
      pure (h:t)

listLiteral :: Parser ListLiteral
listLiteral = operator OperatorBracketsOpen *> do
  optional (operator OperatorBracketsClose) >>= \case
    Just _  -> pure $ ClosedList []
    Nothing -> optional ellipsis >>= \case
      Just expr -> do
        closing
        pure $ OpenList [] expr
      Nothing -> do
        h <- embedding
        go [h]
  where
    closing = do
      optional explicitComma
      operator OperatorBracketsClose
    go elems = do
      optional (try closing) >>= \case
        Just _  -> pure $ ClosedList elems
        Nothing -> do
          explicitComma
          optional ellipsis >>= \case
            Just expr -> do
              closing
              pure $ OpenList elems expr
            Nothing -> do
              elemt <- embedding
              go $ elems ++ [elemt]

-}
