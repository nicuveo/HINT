module Lang.Cue.Parser where

import           Control.Monad
import qualified Control.Monad.State        as MS
import           Data.Functor               ((<&>))
import qualified Data.List                  as L
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (Label, Token, token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char
import Debug.Trace (traceM)
import Control.Monad.Loops (unfoldM)


--------------------------------------------------------------------------------
-- Identifier

newtype Identifier = Identifier Text
  deriving (Show, Eq, Ord)

instance IsString Identifier where
  fromString = Identifier . fromString


--------------------------------------------------------------------------------
-- Tokens

data Token
  = TokenIdentifier    Identifier
  | TokenKeyword       Keyword
  | TokenOperator      Operator
  | TokenAttribute     Attribute
  | TokenInterpolation [InterpolationElement]
  | TokenString        Text
  | TokenInteger       Integer
  | TokenFloat         Double
  deriving (Show, Eq)

data InterpolationElement
  = InterpolationString     Text
  | InterpolationExpression Expression
  deriving (Show, Eq)

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
  | OperatorColon
  | OperatorIsA
  | OperatorOption
  | OperatorNot
  | OperatorParensOpen
  | OperatorParensClose
  | OperatorBracesOpen
  | OperatorBracesClose
  | OperatorBracketsOpen
  | OperatorBracketsClose
  | OperatorEllipsis
  | OperatorPeriod
  | OperatorBottom
  deriving (Show, Eq, Ord, Enum, Bounded)

data Attribute = Attribute
  { attributeName   :: Identifier
  , attributeTokens :: [AttributeToken]
  }
  deriving (Show, Eq)

data AttributeToken
  = AttributeToken    Token
  | AttributeParens   [AttributeToken]
  | AttributeBraces   [AttributeToken]
  | AttributeBrackets [AttributeToken]
  deriving (Show, Eq)


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
  firstC <- letterChar
  rest   <- many $ letterChar <|> digitChar
  pure $ fromMaybe mempty prefix <> T.cons firstC (T.pack rest)

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
  Left kw -> fail "FIXME expected identifier, got keyword"
  Right i -> pure i

-- | Parses an keyword, and skips to the next token on success. Fails without
-- consuming input if the found token is an identifier.
keyword :: Keyword -> Parser Keyword
keyword kw = try $ identifierOrKeyword >>= \case
  Left  k
    | k == kw   -> pure k
    | otherwise -> fail "FIXME expected keyword kw, got other keyword k"
  Right i -> fail "FIXME expected keyword kw, got identifier"

operators :: [Operator] -> Parser Operator
operators = choice . map operator

operator :: Operator -> Parser Operator
operator op = do
  autoComma <- case op of
    OperatorRealComma     -> False <$ char ','
    OperatorNewlineComma  -> False <$ char '\n'
    OperatorEOFComma      -> False <$ eof
    OperatorAdd           -> False <$ char '+'
    OperatorSub           -> False <$ char '-'
    OperatorPow           -> False <$ char '^'
    OperatorMul           -> False <$ char '*'
    OperatorQuo           -> False <$ char '/'
    OperatorArrow         -> False <$ string "<-"
    OperatorLAnd          -> False <$ string "&&"
    OperatorLOr           -> False <$ string "||"
    OperatorAnd           -> False <$ char '&'
    OperatorOr            -> False <$ char '|'
    OperatorEqual         -> False <$ string "=="
    OperatorNotEqual      -> False <$ string "!="
    OperatorMatch         -> False <$ string "=~"
    OperatorNotMatch      -> False <$ string "!~"
    OperatorLTE           -> False <$ string "<="
    OperatorGTE           -> False <$ string ">="
    OperatorLT            -> False <$ char '<'
    OperatorGT            -> False <$ char '>'
    OperatorBind          -> False <$ char '='
    OperatorIsA           -> False <$ string "::"
    OperatorColon         -> False <$ char ':'
    OperatorOption        -> True  <$ char '?'
    OperatorNot           -> False <$ char '!'
    OperatorParensOpen    -> False <$ char '('
    OperatorParensClose   -> True  <$ char ')'
    OperatorBracesOpen    -> False <$ char '{'
    OperatorBracesClose   -> True  <$ char '}'
    OperatorBracketsOpen  -> False <$ char '['
    OperatorBracketsClose -> True  <$ char ']'
    OperatorBottom        -> True  <$ string "_|_"
    OperatorEllipsis      -> True  <$ string "..."
    OperatorPeriod        -> False <$ char '.'
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
          TokenOperator OperatorBracesOpen    -> AttributeBraces   <$> attrTokens OperatorParensClose
          TokenOperator OperatorBracketsOpen  -> AttributeBrackets <$> attrTokens OperatorParensClose
          TokenOperator OperatorParensClose   -> fail "unmatched closing"
          TokenOperator OperatorBracesClose   -> fail "unmatched closing"
          TokenOperator OperatorBracketsClose -> fail "unmatched closing"
          TokenOperator OperatorEOFComma      -> fail "found EOF while parsing attribute tokens"
          TokenInterpolation _                -> fail "not supported in attributes"
          _                                   -> pure $ AttributeToken t
        (at:) <$> attrTokens closing

token :: Parser Token
token = label "token" $ choice
  [ either TokenKeyword TokenIdentifier <$> identifierOrKeyword
  , TokenOperator <$> operators [minBound..maxBound]
  , TokenAttribute <$> attribute
  , either TokenInterpolation TokenString <$> stringOrInterpolationLiteral
  , either TokenFloat TokenInteger <$> numberLiteral
  ]


--------------------------------------------------------------------------------
-- AST

data SourceFile = SourceFile
  { moduleName         :: Maybe Identifier
  , moduleAttributes   :: [Attribute]
  , moduleImports      :: [Import]
  , moduleDeclarations :: [Declaration]
  } deriving (Show, Eq)

data Import = Import
  { importName  :: Maybe Identifier
  , importPath  :: Text
  , importIdent :: Maybe Identifier
  } deriving (Show, Eq)

data Declaration
  = DeclarationField     Field
  | DeclarationEllipsis  Ellipsis
  | DeclarationEmbedding Embedding
  | DeclarationLetClause LetClause
  | DeclarationAttribute Attribute
  deriving (Show, Eq)

data Field = Field
  { fieldLabels     :: NonEmpty Label
  , fieldExpression :: AliasedExpression
  , fieldAttributes :: [Attribute]
  } deriving (Show, Eq)

data Label = Label
  { labelIdentifier :: Maybe Identifier
  , labelExpression :: LabelExpression
  } deriving (Show, Eq)

data LabelExpression
  = LabelName  Optional LabelName
  | LabelAlias AliasedExpression
  deriving (Show, Eq)

data Optional
  = Optional
  | Required
  deriving (Show, Eq)

type LabelName = Text

type Ellipsis = Maybe Expression

data Embedding
  = EmbeddedComprehension Comprehension
  | EmbeddedExpression    AliasedExpression
  deriving (Show, Eq)

data LetClause = LetClause
  { letName       :: Identifier
  , letExpression :: Expression
  } deriving (Show, Eq)

data AliasedExpression = AliasedExpression
  { aeAlias      :: Maybe Identifier
  , aeExpression :: Expression
  } deriving (Show, Eq)

type Comprehension = ()

data Expression
  = Unary          UnaryExpression
  | Multiplication Expression Expression
  | Division       Expression Expression
  | Addition       Expression Expression
  | Subtraction    Expression Expression
  | Equal          Expression Expression
  | NotEqual       Expression Expression
  | Match          Expression Expression
  | NotMatch       Expression Expression
  | LessThan       Expression Expression
  | LessOrEqual    Expression Expression
  | GreaterThan    Expression Expression
  | GreaterOrEqual Expression Expression
  | LogicalAnd     Expression Expression
  | LogicalOr      Expression Expression
  | Unification    Expression Expression
  | Disjunction    Expression Expression
  deriving (Show, Eq)

data UnaryExpression
  = UnaryExpression Operator UnaryExpression
  | PrimaryExpression PrimaryExpression
  deriving (Show, Eq)

data PrimaryExpression
  = PrimaryOperand  Operand
  | PrimarySelector PrimaryExpression Text
  | PrimaryIndex    PrimaryExpression Expression
  | PrimarySlice    PrimaryExpression (Expression, Expression)
  | PrimaryCall     PrimaryExpression [Expression]
  deriving (Show, Eq)

data Operand
  = OperandLiteral    Literal
  | OperandName       QualifiedIdentifier
  | OperandExpression Expression
  deriving (Show, Eq)

data QualifiedIdentifier = QualifiedIdentifier
  { qiPackageName :: Maybe Identifier
  , qiIdentifier  :: Identifier
  } deriving (Show, Eq)

data Literal
  = IntegerLiteral Integer
  | FloatLiteral Double
  | StringLiteral Text
  | BoolLiteral Bool
  | NullLiteral
  | BottomLiteral
  | ListLiteral ListLiteral
  | StructLiteral [Declaration]
  deriving (Show, Eq)

data ListLiteral
   = ClosedList [Embedding]
   | OpenList   [Embedding] Ellipsis
  deriving (Show, Eq)


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
    impPath = do
      path <- stringLiteral
      pure (path, fail "NOT IMPLEMENTED: importSpec")
      -- ensure validity
      -- char '"'
      -- unicodeValue `manyTill_` end
      -- where
      --   end = choice
      --     [ Nothing <$ char '"'
      --     , Just <$> try (char ':' *> identifier <* char '"')
      --     ] <* L.space space1 comment empty


--------------------------------------------------------------------------------
-- Declarations

block :: Parser [Declaration]
block = many $ declaration <* comma

declaration :: Parser Declaration
declaration = choice
  [ DeclarationField     <$> field
  , DeclarationEllipsis  <$> ellipsis
  , DeclarationEmbedding <$> embedding
  , DeclarationLetClause <$> letClause
  , DeclarationAttribute <$> attribute
  ]

field :: Parser Field
field = do
  labels  <- some $ try $ labelElement <* operator OperatorColon
  alias   <- aliasedExpression
  attribs <- many attribute
  pure $ Field (NE.fromList labels) alias attribs

labelElement :: Parser Label
labelElement = do
  name <- optional $ try $ identifier <* operator OperatorBind
  expr <- labelExpr
  pure $ Label name expr

labelExpr :: Parser LabelExpression
labelExpr = labelNameExpr <|> labelAliasExpr
  where
    labelNameExpr = do
      name <- labelName
      opt  <- optional (operator OperatorOption) <&> \case
        Just _  -> Optional
        Nothing -> Required
      pure $ LabelName opt name
    labelAliasExpr = LabelAlias <$> brackets aliasedExpression

labelName :: Parser LabelName
labelName = do
  Identifier n <- identifier
  pure n -- FIXME: simpleStringLit <|> identifier

ellipsis :: Parser Ellipsis
ellipsis = operator OperatorEllipsis *> optional expression

embedding :: Parser Embedding
embedding = choice
  [ EmbeddedComprehension <$> comprehension
  , EmbeddedExpression    <$> aliasedExpression
  ]

letClause :: Parser LetClause
letClause = do
  n <- identifier
  operator OperatorColon
  e <- expression
  pure $ LetClause n e


--------------------------------------------------------------------------------
-- Expressions

expression :: Parser Expression
expression = binaryOp binaryOperators
  where
    binaryOp [] = Unary <$> unaryExpr
    binaryOp (ops:rest) = do
      lhs <- binaryOp rest
      opc <- optional $ choice $ ops <&> \(op, constructor) ->
        try $ constructor <$ operator op
      case opc of
        Nothing -> pure lhs
        Just c  -> do
          rhs <- binaryOp rest
          pure $ c lhs rhs
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
        , (OperatorLT,       LessThan)
        , (OperatorLTE,      LessOrEqual)
        , (OperatorGT,       GreaterThan)
        , (OperatorGTE,      GreaterOrEqual)
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

unaryExpr :: Parser UnaryExpression
unaryExpr = choice [withOp, PrimaryExpression <$> primaryExpr]
  where
    withOp = do
      op   <- operators unaryOperators
      expr <- unaryExpr
      pure $ UnaryExpression op expr
    unaryOperators =
      [ OperatorAdd
      , OperatorSub
      , OperatorMul
      , OperatorNot
      , OperatorNotEqual
      , OperatorMatch
      , OperatorNotMatch
      , OperatorLT
      , OperatorLTE
      , OperatorGT
      , OperatorGTE
      ]

primaryExpr :: Parser PrimaryExpression
primaryExpr = do
  o <- PrimaryOperand <$> operand
  suffixes o
  where
    suffixes pexp = option pexp $ choice
      [ call pexp
      , selector pexp
      , indexOrSlice pexp
      ]
    call pexp = do
      args <- parens $ commaList expression
      suffixes $ PrimaryCall pexp args
    selector pexp = do
      operator OperatorPeriod
      sel <- fail "FIXME" {-identifier-} <|> fail "FIXME" {-simpleStringLit-}
      suffixes $ PrimarySelector pexp sel
    indexOrSlice pexp = do
      low <- expression
      sep <- optional $ operator OperatorColon
      suffixes =<< case sep of
        Nothing ->
          pure $ PrimaryIndex pexp low
        Just _  -> do
          high <- expression
          pure $ PrimarySlice pexp (low, high)

operand :: Parser Operand
operand = choice
  [ OperandName       <$> qualifiedIdentifier
  , OperandExpression <$> parens expression
  , OperandLiteral    <$> literal
  ]

qualifiedIdentifier :: Parser QualifiedIdentifier
qualifiedIdentifier = do
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

comprehension :: Parser ()
comprehension = do
  clauses
  structLiteral
  pure ()

clauses = do
  startClause
  some $ comma *> clause
  where
    startClause = forClause <|> guardClause
    clause = startClause <|> void letClause

forClause = do
  keyword KeywordFor
  low  <- identifier
  high <- optional $ comma *> identifier
  keyword KeywordIn
  expr <- expression
  pure ()

guardClause = do
  keyword KeywordIf
  expr <- expression
  pure ()


--------------------------------------------------------------------------------
-- Literal

literal :: Parser Literal
literal = choice
  [ IntegerLiteral    <$> integerLiteral
  , FloatLiteral      <$> floatLiteral
  , StringLiteral     <$> stringLiteral
  , BoolLiteral True  <$  keyword  KeywordTrue
  , BoolLiteral False <$  keyword  KeywordFalse
  , NullLiteral       <$  keyword  KeywordNull
  , BottomLiteral     <$  operator OperatorBottom
  , StructLiteral     <$> structLiteral
  , ListLiteral       <$> listLiteral
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

integerLiteral :: Parser Integer
integerLiteral = try numberLiteral >>= \case
  Left  _ -> fail "expected integer found float"
  Right n -> pure n

floatLiteral :: Parser Double
floatLiteral = try numberLiteral >>= \case
  Right _ -> fail "expected float found integer"
  Left  f -> pure f

stringOrInterpolationLiteral :: Parser (Either [InterpolationElement] Text)
stringOrInterpolationLiteral = go 0
  where
    go hashCount = do
      let
        closingHashesParser =
          count hashCount (char '#') <|> fail "the number of closing # must match the number in the opening"
      choice
        [ char '#' *> go (hashCount+1)
        , multilineStringLiteral hashCount <* closingHashesParser
        , multilineBytesLiteral  hashCount <* closingHashesParser
        , simpleStringLiteral    hashCount <* closingHashesParser
        , simpleBytesLiteral     hashCount <* closingHashesParser
        ] <|> fail "expecting a string or bytes literal"

simpleStringLiteral :: Int -> Parser (Either [InterpolationElement] Text)
simpleStringLiteral hashCount = do
  char '"'
  res <- charLiteral hashCount False False `manyTill` char '"'
  pure $ postProcess res

multilineStringLiteral :: Int -> Parser (Either [InterpolationElement] Text)
multilineStringLiteral hashCount = do
  string "\"\"\""
  res <- charLiteral hashCount True False `manyTill` string "\"\"\""
  pure $ postProcess res

simpleBytesLiteral :: Int -> Parser (Either [InterpolationElement] Text)
simpleBytesLiteral hashCount = do
  char '\''
  res <- charLiteral hashCount False True `manyTill` char '\''
  pure $ postProcess res

multilineBytesLiteral :: Int -> Parser (Either [InterpolationElement] Text)
multilineBytesLiteral hashCount = do
  string "'''"
  res <- charLiteral hashCount True True `manyTill` string "'''"
  pure $ postProcess res

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
               when isSingleQuotes $
                 fail "unexpected escaped single quote in raw bytes literal"
               pure $ InterpolationString "'"
             '"'  -> do
               unless isSingleQuotes $
                 fail "unexpected escaped double quote in string literal"
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
                 fail "unexpected byte value in string literal"
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
                 fail "unexpected byte value in string literal"
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

stringLiteral :: Parser Text
stringLiteral = try stringOrInterpolationLiteral >>= \case
  Left _  -> fail "expected string found interpolation"
  Right s -> pure s

structLiteral :: Parser [Declaration]
structLiteral = braces block

listLiteral :: Parser ListLiteral
listLiteral = operator OperatorBracketsOpen *> go []
  where
    go elems = do
      ellip <- optional ellipsis
      case ellip of
        Just expr -> do
          optional explicitComma
          operator OperatorBracketsClose
          pure $ OpenList elems expr
        Nothing -> do
          elemt <- embedding
          sep   <- optional explicitComma
          close <- optional $ operator OperatorBracketsClose
          let
            newElems = elems ++ [elemt]
          case (close, sep) of
            (Just _, _) -> pure $ ClosedList newElems
            (_, Just _) -> go newElems
            _           -> fail "FIXME expected comma or closing brackets, found neither"
