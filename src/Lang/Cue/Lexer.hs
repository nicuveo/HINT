{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lang.Cue.Lexer (tokenize) where

import "this" Prelude hiding (exponent)

import Data.Char
import Data.Text                  qualified as T
import Text.Megaparsec            hiding (Label, Token, token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Lang.Cue.Tokens


--------------------------------------------------------------------------------
-- * Running the lexer

tokenize
  :: String -- file name
  -> Text   -- raw input
  -> Either String [Token]
tokenize = left errorBundlePretty ... parse (skipToNextToken False *> many token <* eof)


--------------------------------------------------------------------------------
-- * Tokens

token :: Lexer Token
token = label "token" $ choice
  [ TokenOperator <$> operator
  , TokenAttribute <$> attribute
  , either TokenKeyword TokenIdentifier <$> identifierOrKeyword
  , either TokenInterpolation TokenString <$> stringLiteral
  , either TokenFloat TokenInteger <$> numberLiteral
  , fail "did not find a valid token"
  ]

operator :: Lexer Operator
operator = do
  (op, autoComma) <- choice
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
  skipToNextToken autoComma
  pure op

-- | Parses an attribute, and skips subsequent space.
attribute :: Lexer Attribute
attribute = do
  char '@'
  name <- identifierText
  char '('
  toks <- init <$> attrTokens [OperatorParensClose]
  skipToNextToken True
  pure $ Attribute (Identifier name) toks
  where
    attrTokens closing = case closing of
      []         -> pure []
      (cur:rest) -> do
        t  <- token
        ts <- if t == TokenOperator cur
              then attrTokens rest
              else case t of
                     TokenOperator OperatorParensOpen    -> attrTokens (OperatorParensClose   : closing)
                     TokenOperator OperatorBracesOpen    -> attrTokens (OperatorBracesClose   : closing)
                     TokenOperator OperatorBracketsOpen  -> attrTokens (OperatorBracketsClose : closing)
                     TokenOperator OperatorEOFComma      -> fail "found EOF while parsing attribute tokens"
                     TokenInterpolation _                -> fail "interpolations are not supported in attributes"
                     _                                   -> attrTokens closing
        pure (t:ts)

-- | Parses an identifier, and identify whether it matches a keyword. Skips to
-- the next token.
identifierOrKeyword :: Lexer (Either Keyword Identifier)
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

--------------------------------------------------------------------------------
-- * Literals

stringLiteral :: Lexer (Either [InterpolationElement] Text)
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

simpleStringLiteral :: Int -> Lexer (Either [InterpolationElement] Text)
simpleStringLiteral hashCount = do
  char '"'
  res <- charLiteral hashCount False False `manyTill` char '"'
  pure $ postProcess res

multilineStringLiteral :: Int -> Lexer (Either [InterpolationElement] Text)
multilineStringLiteral hashCount = do
  string "\"\"\"\n"
  res <- charLiteral hashCount True False `manyTill` multilineClosing "\"\"\""
  pure $ postProcess res

simpleBytesLiteral :: Int -> Lexer (Either [InterpolationElement] Text)
simpleBytesLiteral hashCount = do
  char '\''
  res <- charLiteral hashCount False True `manyTill` char '\''
  pure $ postProcess res

multilineBytesLiteral :: Int -> Lexer (Either [InterpolationElement] Text)
multilineBytesLiteral hashCount = do
  string "'''\n"
  res <- charLiteral hashCount True True `manyTill` multilineClosing "'''"
  pure $ postProcess res

charLiteral :: Int -> Bool -> Bool -> Lexer InterpolationElement
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
               pure $ InterpolationString $ T.singleton $ chr $ foldl' (\acc d -> 16*acc + digitToInt d) 0 value
             'U'  -> do
               -- TODO: handle out of bounds hex values
               value <- count 8 hexDigitChar <|> fail "expecting 8 hexadecimal digits after \\U"
               pure $ InterpolationString $ T.singleton $ chr $ foldl' (\acc d -> 16*acc + digitToInt d) 0 value
             'x'  -> do
               -- TODO: handle out of bounds hex values
               unless isSingleQuotes $
                 fail "unexpected hex value in string literal"
               value <- count 2 hexDigitChar <|> fail "expecting 2 hexadecimal digits after \\x"
               pure $ InterpolationString $ T.singleton $ chr $ foldl' (\acc d -> 16*acc + digitToInt d) 0 value
             '(' -> do
               -- TODO: what about newlines?
               skipToNextToken True
               tks <- init <$> interpolationTokens [OperatorParensClose]
               pure $ InterpolationExpression tks
             oct -> do
               unless isSingleQuotes $
                 fail "unexpected octal value in string literal"
               value <- count 2 octDigitChar <|> fail "expecting 3 octal digits after \\"
               pure $ InterpolationString $ T.singleton $ chr $ foldl' (\acc d -> 8*acc + digitToInt d) 0 $ oct:value
     | otherwise -> pure $ InterpolationString $ T.singleton c
  where
    -- similarly to attrTokens, this expects a balanced set of tokens
    -- but, unlike attributes, it doesn't reject interpolations
    interpolationTokens closing = case closing of
      []         -> pure []
      (cur:rest) -> do
        t  <- token
        ts <- if t == TokenOperator cur
              then interpolationTokens rest
              else case t of
                     TokenOperator OperatorParensOpen    -> interpolationTokens (OperatorParensClose   : closing)
                     TokenOperator OperatorBracesOpen    -> interpolationTokens (OperatorBracesClose   : closing)
                     TokenOperator OperatorBracketsOpen  -> interpolationTokens (OperatorBracketsClose : closing)
                     TokenOperator OperatorEOFComma      -> fail "found EOF while parsing attribute tokens"
                     _                                   -> interpolationTokens closing
        pure (t:ts)

-- | Parses a number token, and skips subsequent space.
numberLiteral :: Lexer (Either Double Integer)
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
      pure $ foldl' (\acc d ->  2*acc + fromIntegral (digitToInt d)) 0 value
    octal = do
      string "0o"
      value <- some octDigitChar
      pure $ foldl' (\acc d ->  8*acc + fromIntegral (digitToInt d)) 0 value
    hex = do
      string "0x" <|> string "0X"
      value <- some hexDigitChar
      pure $ foldl' (\acc d -> 16*acc + fromIntegral (digitToInt d)) 0 value
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

--------------------------------------------------------------------------------
-- * Internal helpers

type Lexer = Parsec Void Text

-- | Skips all following whitespace and comments. However, in accordance to
-- section "Commas" of the reference, this stops at a newline where a comma
-- should be inserted. The next token will then identify that newline as a
-- comma.
skipToNextToken :: Bool -> Lexer ()
skipToNextToken autoComma = do
  L.space hspace1 comment empty
  remainingInput <- getInput
  unless (autoComma && (T.null remainingInput || T.head remainingInput == '\n')) $
    L.space space1 comment empty
  where
    comment = L.skipLineComment "//"

-- | Parses the raw text that matches an identifier, but doesn't perform any
-- further check. Does NOT skip to the next token!
identifierText :: Lexer Text
identifierText = do
  prefix <- optional $ string "#" <|> string "_#"
  name   <- identifierName
  pure $ fromMaybe mempty prefix <> name

-- | Parses the raw text that matches an identifier's name (without leading
-- prefixes), but doesn't perform any further check. Does NOT skip to the next
-- token!
identifierName :: Lexer Text
identifierName = do
  firstC <- letter
  rest   <- many $ letter <|> digitChar
  pure $ T.cons firstC (T.pack rest)
  where
    letter = char '_' <|> char '$' <|> letterChar

-- | Attempts to match a multiline closing. Backtracks on error!
multilineClosing :: Text -> Lexer ()
multilineClosing s = try $ void $ char '\n' >> hspace >> string s

-- | Fuses the individual elements of a string literal. If there are no
-- interpolations in the literal, it fuses all elements in one block of 'Text',
-- returns a heterogeneous list otherwise.
postProcess :: [InterpolationElement] -> Either [InterpolationElement] Text
postProcess l = case foldr fuse [] l of
  []                      -> Right ""
  [InterpolationString t] -> Right t
  heterogeneousList       -> Left heterogeneousList
  where
    fuse (InterpolationString t1) (InterpolationString t2 : r) = InterpolationString (t1 <> t2) : r
    fuse x r = x : r
