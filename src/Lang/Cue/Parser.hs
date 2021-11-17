module Lang.Cue.Parser where

import           Control.Monad
import           Control.Monad.Loops        (unfoldM)
import qualified Control.Monad.State        as MS
import           Data.Char
import           Data.Functor               ((<&>))
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Debug.Trace                (traceM)
import           Text.Megaparsec            hiding (Label, Token, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Lang.Cue.Grammar


--------------------------------------------------------------------------------
-- Lexer

-- | Skips all following whitespace and comments. However, in accordance to
-- section "Commas" of the reference, this stops at a newline where a comma
-- should be inserted. The next token will then identify that newline as a
-- comma.
skipToNextToken :: Bool -> Parser ()
skipToNextToken autoComma = do
  L.space hspace1 comment empty
  remainingInput <- getInput
  unless (autoComma && (T.null remainingInput || T.head remainingInput == '\n')) $
    L.space space1 comment empty
  where
    comment = L.skipLineComment "//"

-- | Parses the raw text that matches an identifier, but doesn't perforn any
-- further check. Does NOT skip to the next token!
identifierText :: Parser Text
identifierText = try $ do
  prefix <- optional $ string "#" <|> string "_#"
  firstC <- letter
  rest   <- many $ letter <|> digitChar
  pure $ fromMaybe mempty prefix <> T.cons firstC (T.pack rest)
  where
    letter = char '_' <|> letterChar

-- | Parses an identifier, and identify whether it matches a keyword. Skips to
-- the next token.
identifierOrKeyword :: Parser (Either Keyword Identifier)
identifierOrKeyword = do
  res <- identifierText <&> \case
    "package" -> Left KeywordPackage
    "import"  -> Left KeywordImport
    "null"    -> Left KeywordNull
    "true"    -> Left KeywordTrue
    "false"   -> Left KeywordFalse
    "for"     -> Left KeywordFor
    "in"      -> Left KeywordIn
    "if"      -> Left KeywordIf
    "let"     -> Left KeywordLet
    ident     -> Right $ Identifier ident
  skipToNextToken True
  pure res

-- | Parses an identifier, and skips to the next token on success. Fails without
-- consuming input if the found identifier is a keyword.
identifier :: Parser Identifier
identifier = try $ identifierOrKeyword >>= \case
  Left kw -> fail $ "expected identifier, got keyword " ++ show kw
  Right i -> pure i

-- | Parses an keyword, and skips to the next token on success. Fails without
-- consuming input if the found token is an identifier.
keyword :: Keyword -> Parser Keyword
keyword kw = try $ identifierOrKeyword >>= \case
  Left  k
    | k == kw   -> pure k
    | otherwise -> fail $ "expected keyword " ++ show kw ++ ", got other keyword " ++ show k
  Right i -> fail $ "expected keyword " ++ show kw ++ ", got identifier"

-- | Parses an operator, and does NOT skip following space.
nextOperator :: Parser (Operator, Bool)
nextOperator = choice
  [ (OperatorEOFComma,      False) <$ eof
  , (OperatorRealComma,     False) <$ char ','
  , (OperatorNewlineComma,  False) <$ char '\n'
  , (OperatorAdd,           False) <$ char '+'
  , (OperatorSub,           False) <$ char '-'
  , (OperatorPow,           False) <$ char '^'
  , (OperatorMul,           False) <$ char '*'
  , (OperatorQuo,           False) <$ char '/'
  , (OperatorArrow,         False) <$ string "<-"
  , (OperatorLAnd,          False) <$ string "&&"
  , (OperatorLOr,           False) <$ string "||"
  , (OperatorAnd,           False) <$ char '&'
  , (OperatorOr,            False) <$ char '|'
  , (OperatorEqual,         False) <$ string "=="
  , (OperatorNotEqual,      False) <$ string "!="
  , (OperatorMatch,         False) <$ string "=~"
  , (OperatorNotMatch,      False) <$ string "!~"
  , (OperatorLTE,           False) <$ string "<="
  , (OperatorGTE,           False) <$ string ">="
  , (OperatorLT,            False) <$ char '<'
  , (OperatorGT,            False) <$ char '>'
  , (OperatorBind,          False) <$ char '='
  , (OperatorIsA,           False) <$ string "::"
  , (OperatorColon,         False) <$ char ':'
  , (OperatorOption,        True ) <$ char '?'
  , (OperatorNot,           False) <$ char '!'
  , (OperatorParensOpen,    False) <$ char '('
  , (OperatorParensClose,   True ) <$ char ')'
  , (OperatorBracesOpen,    False) <$ char '{'
  , (OperatorBracesClose,   True ) <$ char '}'
  , (OperatorBracketsOpen,  False) <$ char '['
  , (OperatorBracketsClose, True ) <$ char ']'
  , (OperatorBottom,        True ) <$ string "_|_"
  , (OperatorEllipsis,      True ) <$ string "..."
  , (OperatorPeriod,        False) <$ char '.'
  ]

operators :: [Operator] -> Parser Operator
operators ops = do
  (op, autoComma) <- try $ do
    (op, autoComma) <- nextOperator
    unless (op `elem` ops) $
      fail $ "expected one of " <> show ops <> ", got " <> show op
    pure (op, autoComma)
  skipToNextToken autoComma
  pure op

operator :: Operator -> Parser Operator
operator expected = do
  (op, autoComma) <- try $ do
    (op, autoComma) <- nextOperator
    unless (op == expected) $
      fail $ "expected " <> show expected <>  ", got "<> show op
    pure (op, autoComma)
  skipToNextToken autoComma
  pure op

anyOperator :: Parser Operator
anyOperator = do
  (op, autoComma) <- nextOperator
  skipToNextToken autoComma
  pure op

attribute :: Parser Attribute
attribute = do
  char '@'
  name <- identifierText
  operator OperatorParensOpen
  toks <- attrTokens OperatorParensClose
  skipToNextToken True
  pure $ Attribute (Identifier name) toks
  where
    attrTokens closing = do
      t <- token
      if t == TokenOperator closing
      then pure []
      else do
        at <- case t of
          TokenOperator OperatorParensOpen    -> AttributeParens   <$> attrTokens OperatorParensClose
          TokenOperator OperatorBracesOpen    -> AttributeBraces   <$> attrTokens OperatorBracesClose
          TokenOperator OperatorBracketsOpen  -> AttributeBrackets <$> attrTokens OperatorBracketsClose
          TokenOperator OperatorParensClose   -> fail "unmatched closing"
          TokenOperator OperatorBracesClose   -> fail "unmatched closing"
          TokenOperator OperatorBracketsClose -> fail "unmatched closing"
          TokenOperator OperatorEOFComma      -> fail "found EOF while parsing attribute tokens"
          TokenInterpolation _                -> fail "interpolations are not supported in attributes"
          _                                   -> pure $ AttributeToken t
        (at:) <$> attrTokens closing

token :: Parser Token
token = label "token" $ choice
  [ TokenOperator <$> anyOperator
  , TokenAttribute <$> attribute
  , either TokenFloat TokenInteger <$> numberLiteral
  , either TokenKeyword TokenIdentifier <$> identifierOrKeyword
  , either TokenInterpolation TokenString <$> stringLiteral
  , fail "did not find a valid token"
  ]


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
    go expr [label]      = Field label expr []
    go expr (label:subs) = Field label (mkExpr $ go expr subs) []

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
    binaryOp (ops:rest) = do
      lhs <- binaryOp rest
      opc <- many $ choice $ ops <&> \(op, constructor) -> do
        rhs <- operator op *> binaryOp rest
        pure (constructor, rhs)
      pure $ foldl (\l (o, r) -> o l r) lhs opc
    binaryOperators =
      [ -- precedence 1
        [ (OperatorOr, Disjunction)
        ]
      , -- precedence 2
        [ (OperatorAnd, Unification)
        ]
      , -- precedence 3
        [ (OperatorLOr, LogicalOr)
        ]
      , -- precedence 4
        [ (OperatorLAnd, LogicalAnd)
        ]
      , -- precedence 5
        [ (OperatorEqual,    Equal)
        , (OperatorNotEqual, NotEqual)
        , (OperatorMatch,    Match)
        , (OperatorNotMatch, NotMatch)
        , (OperatorLTE,      LessOrEqual)
        , (OperatorLT,       LessThan)
        , (OperatorGTE,      GreaterOrEqual)
        , (OperatorGT,       GreaterThan)
        ]
      , -- precedence 6
        [ (OperatorAdd, Addition)
        , (OperatorSub, Subtraction)
        ]
      , -- precedence 7
        [ (OperatorMul, Multiplication)
        , (OperatorQuo, Division)
        ]
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
    Just pn -> do
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
      r <- oneOf @[] "KMGTP"
      i <- optional (char 'i') <&> \case
        Just _  -> 1024
        Nothing -> 1000
      pure $ case r of
        'K' -> i**1
        'M' -> i**2
        'G' -> i**3
        'T' -> i**4
        'P' -> i**5

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
           c <- oneOf @[] "abfnrtv/\\'\"(uUx01234567" <|> fail "invalid escape character"
           case c of
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
               e <- expression
               char ')' <|> fail "expecting closing paren at the end of interpolation"
               pure $ InterpolationExpression e
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
    fuse (InterpolationString t1) (InterpolationString t2 : l) = InterpolationString (t1<>t2) : l
    fuse x l = x : l

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
