{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lang.Cue.Lexer (tokenize) where

import "this" Prelude             hiding (exponent)

import Data.Char
import Data.Set                   ()
import Data.Text                  qualified as T
import Text.Megaparsec            hiding (Token, token, tokens)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Lang.Cue.Error
import Lang.Cue.HKD
import Lang.Cue.Location          hiding (getOffset)
import Lang.Cue.Tokens


--------------------------------------------------------------------------------
-- * Running the lexer

tokenize
  :: String
  -> Text
  -> Either [Error] [Token WithLocation]
tokenize filename code =
  bimap (foldMap report . bundleErrors) (map annotate) $
    parse tokens filename code
  where
    tokens :: Lexer [Token WithOffset]
    tokens = do
      skipToNextToken False
      result <- token `manyTill` eof
      eofTok <- token
      pure $ concat result <> eofTok

    codeLines :: [Text]
    codeLines = T.lines code

    mkLocation :: Int -> a -> WithLocation a
    mkLocation o a = withLocation (Location filename codeLines o) a

    annotate :: Token WithOffset -> Token WithLocation
    annotate = ffmap \(WithOffset (o, a)) -> mkLocation o a

    report :: ParseError Text e -> [WithLocation ErrorInfo]
    report = \case
      FancyError o errs -> do
        err <- toList errs
        pure $
          mkLocation o $
            case err of
              ErrorFail s -> LexerCustomError s
              _           -> unreachable
      TrivialError o got want ->
        pure $
          mkLocation o $
            LexerTokenError (fmap display got) (map display $ toList want)

    display :: ErrorItem Char -> String
    display = \case
      Tokens s   -> toList s
      Label  s   -> toList s
      EndOfInput -> "end of input"


--------------------------------------------------------------------------------
-- * Tokens

token :: Lexer [Token WithOffset]
token = label "token" $ choice
  [ try stringLiteral
  , pure . either TokenKeyword TokenIdentifier . flipE <$> identifierOrKeyword
  , pure . either TokenFloat TokenInteger      . flipE <$> numberLiteral
  , pure . TokenAttribute <$> attribute
  , pure . TokenOperator <$> operator
  , fail "did not find a valid token"
  ]

operator :: Lexer (WithOffset Operator)
operator = label "operator" $ addOffset do
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
attribute :: Lexer (WithOffset Attribute)
attribute = label "attribute" $ addOffset do
  char '@'
  name <- identifierText
  char '('
  -- get a copy of the rest of the file and the current offset
  buffer <- getInput
  start  <- getOffset
  -- go through all tokens, discard the closing paren
  attrTokens [OperatorParensClose]
  -- get current position, and extract raw text
  end <- getOffset
  let rawAttribute = T.take (end - start - 1) buffer
  -- skip to next token and return attribute
  skipToNextToken True
  pure $ Attribute (Identifier name) rawAttribute
  where
    attrTokens closing = case closing of
      []         -> pure ()
      (cur:rest) -> do
        t  <- reify discardOffset . head <$> token
        if t == TokenOperator cur
        then attrTokens rest
        else case t of
               TokenOperator OperatorParensOpen   -> attrTokens (OperatorParensClose   : closing)
               TokenOperator OperatorBracesOpen   -> attrTokens (OperatorBracesClose   : closing)
               TokenOperator OperatorBracketsOpen -> attrTokens (OperatorBracketsClose : closing)
               TokenOperator OperatorEOFComma     -> fail "found EOF while parsing attribute tokens"
               TokenInterpolationBegin _          -> fail "interpolations are not supported in attributes"
               _                                  -> attrTokens closing

-- | Parses an identifier, and identify whether it matches a keyword. Skips to
-- the next token.
identifierOrKeyword :: Lexer (WithOffset (Either Keyword Identifier))
identifierOrKeyword = addOffset do
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

stringLiteral :: Lexer [Token WithOffset]
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

simpleStringLiteral :: Int -> Lexer [Token WithOffset]
simpleStringLiteral hashCount = do
  b <- getOffset
  char '"'
  let delim = replicate hashCount '#' <> "\""
  res <- charLiteral delim hashCount False False `manyTill` char '"'
  e <- subtract 1 <$> getOffset
  pure $ postProcess delim b e res

multilineStringLiteral :: Int -> Lexer [Token WithOffset]
multilineStringLiteral hashCount = do
  b <- getOffset
  string "\"\"\"\n"
  let delim = replicate hashCount '#' <> "\"\"\""
  res <- charLiteral delim hashCount True False `manyTill` multilineClosing "\"\"\""
  e <- subtract 3 <$> getOffset
  pure $ postProcess delim b e res

simpleBytesLiteral :: Int -> Lexer [Token WithOffset]
simpleBytesLiteral hashCount = do
  b <- getOffset
  char '\''
  let delim = replicate hashCount '#' <> "'"
  res <- charLiteral delim hashCount False True `manyTill` char '\''
  e <- subtract 1 <$> getOffset
  pure $ postProcess delim b e res

multilineBytesLiteral :: Int -> Lexer [Token WithOffset]
multilineBytesLiteral hashCount = do
  b <- getOffset
  string "'''\n"
  let delim = replicate hashCount '#' <> "'''"
  res <- charLiteral delim hashCount True True `manyTill` multilineClosing "'''"
  e <- subtract 3 <$> getOffset
  pure $ postProcess delim b e res

charLiteral :: String -> Int -> Bool -> Bool -> Lexer [Token WithOffset]
charLiteral delimiter hashCount allowNewline isSingleQuotes = do
  o <- getOffset
  c <- anySingle
  let mkT = TokenString . withOffset o . (delimiter,)
  if | c == '\n' && not allowNewline -> fail "unterminated in single-line literal"
     | c == '\\' -> optional (count hashCount $ char '#') >>= \case
         Nothing -> do
           s <- many $ char '#'
           pure [mkT $ T.pack $ '\\' : s]
         Just _  -> do
           e <- oneOf @[] "abfnrtv/\\'\"(uUx01234567" <|> fail "invalid escape character"
           case e of
             'a'  -> pure [mkT "\a"]
             'b'  -> pure [mkT "\b"]
             'f'  -> pure [mkT "\f"]
             'n'  -> pure [mkT "\n"]
             'r'  -> pure [mkT "\r"]
             't'  -> pure [mkT "\t"]
             'v'  -> pure [mkT "\v"]
             '/'  -> pure [mkT "/" ]
             '\\' -> pure [mkT "\\"]
             '\'' -> do
               unless isSingleQuotes $
                 fail "unexpected escaped single quote in string literal"
               pure [mkT "'"]
             '"'  -> do
               when isSingleQuotes $
                 fail "unexpected escaped double quote in bytes literal"
               pure [mkT "\""]
             'u'  -> do
               -- TODO: handle out of bounds hex values
               value <- count 4 hexDigitChar <|> fail "expecting 4 hexadecimal digits after \\u"
               pure [mkT $ T.singleton $ chr $ foldl' (\acc d -> 16*acc + digitToInt d) 0 value]
             'U'  -> do
               -- TODO: handle out of bounds hex values
               value <- count 8 hexDigitChar <|> fail "expecting 8 hexadecimal digits after \\U"
               pure [mkT $ T.singleton $ chr $ foldl' (\acc d -> 16*acc + digitToInt d) 0 value]
             'x'  -> do
               -- TODO: handle out of bounds hex values
               unless isSingleQuotes $
                 fail "unexpected hex value in string literal"
               value <- count 2 hexDigitChar <|> fail "expecting 2 hexadecimal digits after \\x"
               pure [mkT $ T.singleton $ chr $ foldl' (\acc d -> 16*acc + digitToInt d) 0 value]
             '(' -> do
               -- TODO: what about newlines?
               skipToNextToken True
               ib <- getOffset
               tks <- init <$> interpolationTokens [OperatorParensClose]
               eb <- subtract 1 <$> getOffset
               pure $ [TokenInterpolationExprBegin $ withOffset ib ()] <> tks <> [TokenInterpolationExprEnd $ withOffset eb ()]
             oct -> do
               unless isSingleQuotes $
                 fail "unexpected octal value in string literal"
               value <- count 2 octDigitChar <|> fail "expecting 3 octal digits after \\"
               pure [mkT $ T.singleton $ chr $ foldl' (\acc d -> 8*acc + digitToInt d) 0 $ oct:value]
     | otherwise -> pure [mkT $ T.singleton c]
  where
    -- similarly to attrTokens, this expects a balanced set of tokens
    -- but, unlike attributes, it doesn't reject interpolations
    interpolationTokens closing = case closing of
      []         -> pure []
      (cur:rest) -> do
        toks@(t:_) <- token
        following <- case t of
          TokenOperator (WithOffset (_, op))
            | op == cur                  -> interpolationTokens rest
            | op == OperatorParensOpen   -> interpolationTokens (OperatorParensClose   : closing)
            | op == OperatorBracesOpen   -> interpolationTokens (OperatorBracesClose   : closing)
            | op == OperatorBracketsOpen -> interpolationTokens (OperatorBracketsClose : closing)
            | op == OperatorEOFComma     -> fail "found EOF while parsing attribute tokens"
          _                              -> interpolationTokens closing
        pure $ toks <> following

-- | Parses a number token, and skips subsequent space.
numberLiteral :: Lexer (WithOffset (Either Double Integer))
numberLiteral = label "number" $ addOffset do
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
-- interpolations in the literal, it fuses all elements in one string token,
-- and returns a heterogeneous list otherwise.
postProcess :: String -> Offset -> Offset -> [[Token WithOffset]] -> [Token WithOffset]
postProcess d b e l = case foldr fuse [] l of
  [] -> [TokenString $ withOffset b (d, "")]
  r@[TokenString _]  -> r
  r -> [TokenInterpolationBegin $ withOffset b ()] <> r <> [TokenInterpolationEnd $ withOffset e ()]
  where
    fuse
      [TokenString (WithOffset (o, (x, s1)))]
      (TokenString (WithOffset (_, (_, s2))) : r) =
       TokenString (WithOffset (o, (x, s1 <> s2))) : r
    fuse x r = x <> r

addOffset :: Lexer a -> Lexer (WithOffset a)
addOffset = liftA2 withOffset (getOffset)

flipE :: WithOffset (Either a b) -> Either (WithOffset a) (WithOffset b)
flipE (WithOffset (o, e)) = case e of
  Left  l -> Left  $ withOffset o l
  Right r -> Right $ withOffset o r
