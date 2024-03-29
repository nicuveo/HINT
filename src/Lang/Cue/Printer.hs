module Lang.Cue.Printer
  ( display
  , displayLazy
  , toString
  ) where

import "this" Prelude

-- import Data.Char
-- import Data.List              (intersperse)
-- import Data.List              qualified as L
-- import Data.List.NonEmpty     qualified as NE
-- import Data.Scientific
import Data.Text              qualified as T
import Data.Text.Lazy         (toStrict)
import Data.Text.Lazy         qualified as TL
import Data.Text.Lazy.Builder

-- import Lang.Cue.AST
-- import Lang.Cue.Tokens
-- import Lang.Cue.Value


--------------------------------------------------------------------------------
-- API

class Printer a where
  build :: Int -> a -> Builder

display :: Printer a => a -> T.Text
display = toStrict . displayLazy

displayLazy :: Printer a => a -> TL.Text
displayLazy = toLazyText . build 0

toString :: Printer a => a -> String
toString = TL.unpack . displayLazy

{-

--------------------------------------------------------------------------------
-- Base instances

instance Printer Builder where
  build _ = id

instance Printer Integer where
  build _ = fromString . show

instance Printer Scientific where
  build _ = fromString . show

instance Printer Bool where
  build _ False = "false"
  build _ True  = "true"


-}

--------------------------------------------------------------------------------
-- Tokens

{-
instance Printer (Token Identity) where
  build i = \case
    TokenIdentifier    (Identity x) -> build i x
    TokenKeyword       (Identity x) -> build i x
    TokenOperator      (Identity x) -> build i x
    TokenAttribute     (Identity x) -> build i x
    TokenInterpolation (Identity x) -> buildString i $ Left  x
    TokenString        (Identity x) -> buildString i $ Right x
    TokenInteger       (Identity x) -> build i x
    TokenFloat         (Identity x) -> build i x

instance Printer [Token Identity] where
  build i = withSpaces i

instance Printer Identifier where
  build _ (Identifier t) = fromText t

instance Printer Keyword where
  build _ = \case
    KeywordPackage -> "package"
    KeywordImport  -> "import"
    KeywordNull    -> "null"
    KeywordTrue    -> "true"
    KeywordFalse   -> "false"
    KeywordFor     -> "for"
    KeywordIn      -> "in"
    KeywordIf      -> "if"
    KeywordLet     -> "let"

instance Printer Operator where
  build _ = \case
    OperatorRealComma     -> ","
    OperatorNewlineComma  -> "," -- on purpose
    OperatorEOFComma      -> mempty
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
    OperatorColon         -> ":"
    OperatorIsA           -> "::"
    OperatorOption        -> "?"
    OperatorNot           -> "!"
    OperatorParensOpen    -> "("
    OperatorParensClose   -> ")"
    OperatorBracesOpen    -> "{"
    OperatorBracesClose   -> "}"
    OperatorBracketsOpen  -> "["
    OperatorBracketsClose -> "]"
    OperatorEllipsis      -> "..."
    OperatorPeriod        -> "."
    OperatorBottom        -> "_|_"

instance Printer Attribute where
  build i (Attribute name raw) =
    "@" <> build i name <> "(" <> fromText raw <> ")"

instance Printer InterpolationElement where
  build i = \case
    InterpolationString     t -> encodeText t
    InterpolationExpression e -> "\\(" <> build i e <> ")"
-}


--------------------------------------------------------------------------------
-- AST

{-
instance Printer SourceFile where
  build i (SourceFile ident attribs imports decls) =
    mconcat $ map (<> "\n") $ concat
      [ build i <$> attribs
      , maybeToList $ ident <&> \n -> "package " <> build i n
      , build i <$> imports
      , build i <$> decls
      ]

instance Printer Import where
  build i (Import name path ident) = "import "
    <> maybe "" (\x -> build i x <> " ") name
    <> "\""
    <> fromText path
    <> maybe "" (\x -> ":" <> build i x) ident
    <> "\""

instance Printer Declaration where
  build i = \case
    DeclarationField     x -> build i x
    DeclarationEllipsis  x -> build i x
    DeclarationEmbedding x -> build i x
    DeclarationLetClause x -> build i x
    DeclarationAttribute x -> build i x

instance Printer Field where
  build i (Field l e a) = build i l <> ": " <> build i e <> if null a
    then ""
    else " " <> withSpaces i a

instance Printer Label where
  build i (Label alias expr) = maybe "" (\a -> build i a <> " = ") alias <> build i expr

instance Printer LabelExpression where
  build i = \case
    LabelIdentifier name opt   -> build i name <> build i opt
    LabelString     name opt   -> buildString i name <> build i opt
    LabelConstraint constraint -> "[" <> build i constraint <> "]"

instance Printer Optional where
  build _ = \case
    Optional -> "?"
    Required -> ""

instance Printer Ellipsis where
  build i e = "..." <> maybe "" (\x -> build i x) e

instance Printer Embedding where
  build i = \case
    EmbeddedComprehension c -> build i c
    EmbeddedExpression    a -> build i a

instance Printer LetClause where
  build i (LetClause ident expr) =
    "let " <> build i ident <> " = " <> build i expr

instance Printer AliasedExpression where
  build i (AliasedExpression alias expr) =
    maybe "" (\a -> build i a <> " = ") alias <> build i expr

instance Printer Comprehension where
  build i (Comprehension clauses decl) =
    mconcat (intersperse " " $ NE.toList $ build i <$> clauses) <> " " <> makeBlock i decl

instance Printer ComprehensionClause where
  build i = \case
    ComprehensionFor        j   e -> "for " <> build i j <> " in " <> build i e
    ComprehensionIndexedFor j k e -> "for " <> build i j <> ", " <> build i k <> " in " <> build i e
    ComprehensionIf             e -> "if "  <> build i e
    ComprehensionLet        j   e -> "let " <> build i j <> " = " <> build i e

instance Printer Expression where
  build i = \case
    Unary          u      -> build i u
    Multiplication l r vs -> buildE " * "  l r vs
    Division       l r vs -> buildE " / "  l r vs
    Addition       l r vs -> buildE " + "  l r vs
    Subtraction    l r vs -> buildE " - "  l r vs
    Equal          l r vs -> buildE " == " l r vs
    NotEqual       l r vs -> buildE " != " l r vs
    Match          l r vs -> buildE " =~ " l r vs
    NotMatch       l r vs -> buildE " !~ " l r vs
    LessThan       l r vs -> buildE " < "  l r vs
    LessOrEqual    l r vs -> buildE " <= " l r vs
    GreaterThan    l r vs -> buildE " > "  l r vs
    GreaterOrEqual l r vs -> buildE " >= " l r vs
    LogicalAnd     l r vs -> buildE " && " l r vs
    LogicalOr      l r vs -> buildE " || " l r vs
    Unification    l r vs -> buildE " & "  l r vs
    Disjunction    l r vs -> buildE " | "  l r vs
    where
      buildE op l r vs = L.foldl' (step op) (build i l) (r:vs)
      step op l r = l <> op <> build i r

instance Printer UnaryExpression where
  build i (UnaryExpression ops pe) =
    withSpaces i ops <> build i pe

instance Printer PrimaryExpression where
  build i = \case
    PrimaryOperand  o       -> build i o
    PrimarySelector e s     -> build i e <> " . " <> either (build i) (buildString i) s
    PrimaryIndex    e j     -> build i e <> "[" <> build i j <> "]"
    PrimarySlice    e (l,h) -> build i e <> "[" <> build i l <> ":" <> build i h <> "]"
    PrimaryCall     e args  -> build i e <> "(" <> withCommas i args <> ")"

instance Printer Operand where
  build i = \case
    OperandLiteral    l -> build i l
    OperandName       n -> build i n
    OperandExpression e -> "(" <> build i e <> ")"

instance Printer QualifiedIdentifier where
  build i (QualifiedIdentifier pn ident) =
    maybe "" (\n -> build i n <> ".") pn <> build i ident

instance Printer Literal where
  build i = \case
    IntegerLiteral n -> build i n
    FloatLiteral d   -> build i d
    StringLiteral s  -> buildString i s
    BoolLiteral b    -> build i b
    NullLiteral      -> "null"
    BottomLiteral    -> "_|_"
    ListLiteral l    -> build i l
    StructLiteral ds -> makeBlock i ds

instance Printer ListLiteral where
  build i = \case
    ClosedList []    -> "[]"
    ClosedList [x]   -> "[" <> build i x <> "]"
    ClosedList (x:r) -> mconcat
      [ "[ " <> build (i + 1) x <> ",\n" <> indent
      , mconcat $ r <&> \y -> "  " <> build (i + 1) y <> ",\n" <> indent
      , "]"
      ]
    OpenList []    ell -> "[" <> build i ell <> "]"
    OpenList (x:r) ell -> mconcat
      [ "[ " <> build (i + 1) x <> ",\n" <> indent
      , mconcat $ r <&> \y -> "  " <> build (i + 1) y <> ",\n" <> indent
      , "  " <> build (i + 1) ell <> ",\n" <> indent
      , "]"
      ]
    where
      indent = fromString $ replicate (2*i) ' '
-}


--------------------------------------------------------------------------------
-- Value

{-

instance Printer Void where
  build = const absurd

instance Printer v => Printer (CoreValue v) where
  build i = \case
    Type t -> case t of
      BooleanType  -> "bool"
      IntegerType  -> "int"
      FloatType    -> "float"
      StringType   -> "string"
      BytesType    -> "bytes"
      StructType n -> build i n
    Atom a -> case a of
      BooleanAtom b -> build i b
      IntegerAtom n -> build i n
      FloatAtom   f -> build i f
      StringAtom  t -> buildText t
      BytesAtom   t -> buildText t
    Bound b -> case b of
      IntegerBound o -> buildOrdered (build i) o
      FloatBound   o -> buildOrdered (build i) o
      StringBound  o -> buildOrdered (buildText) o
      BytesBound   o -> buildOrdered (buildText) o
      RegexBound   m -> unify i $ m <&> \(p, e) ->
        if e
        then "=~ \"" <> encodeText p <> "\""
        else "!~ \"" <> encodeText p <> "\""
    Bottom _ -> "_|_" -- FIXME: print reason?
    Top -> "_"
    Null -> "null"
    Union values -> disjoin i $ toList values
    Function identifier -> build i identifier
    WithDefault v d -> "⟨" <> build i v <> ", " <> build i d <> "⟩"
    where
      buildText t = "\"" <> encodeText t <> "\""
      buildOrdered :: (a -> Builder) -> OrderedBound a -> Builder
      buildOrdered f (OrderedBound l u d) = unify i $ concat
        [ catMaybes
          [ case l of
            Open        -> Nothing
            Inclusive a -> Just $ ">= " <> f a
            Exclusive a -> Just $ "> "  <> f a
          , case u of
            Open        -> Nothing
            Inclusive a -> Just $ "<= " <> f a
            Exclusive a -> Just $ "< "  <> f a
          ]
        , d <&> \a -> "!= " <> f a
        ]


--------------------------------------------------------------------------------
-- Helpers

makeBlock :: Printer a => Int -> [a] -> Builder
makeBlock _ []  = "{}"
makeBlock i [x] = "{" <> build i x <> "}"
makeBlock i l   = mconcat
  [ "{\n"
  , mconcat $ l <&> \x -> indent <> "  " <> build (i + 1) x <> ",\n"
  , indent
  , "}"
  ]
  where indent = fromString $ replicate (2*i) ' '

unify :: Printer a => Int -> [a] -> Builder
unify i = mconcat . intersperse " & " . map (build i)

disjoin :: Printer a => Int -> [a] -> Builder
disjoin i = mconcat . intersperse " | " . map (build i)

withCommas :: Printer a => Int -> [a] -> Builder
withCommas i = mconcat . intersperse ", " . map (build i)

withSpaces :: Printer a => Int -> [a] -> Builder
withSpaces i = mconcat . intersperse " " . map (build i)

buildString :: Int -> StringLiteral -> Builder
buildString i = \case
  Left  elts -> "\"" <> foldMap (build i) elts <> "\""
  Right strl -> "\"" <> encodeText strl <> "\""

encodeText :: Text -> Builder
encodeText = T.foldl' escapeChar mempty
  where
    hexadecimal n
      | n < 16    = [intToDigit n]
      | otherwise = hexadecimal (n `quot` 16) ++ [intToDigit (n `rem` 16)]
    escapeChar b c = b <>
      if | c == '\a' -> "\\a"
         | c == '\b' -> "\\b"
         | c == '\f' -> "\\f"
         | c == '\n' -> "\\n"
         | c == '\r' -> "\\r"
         | c == '\t' -> "\\t"
         | c == '\v' -> "\\v"
         | c == '\\' -> "\\\\"
         | c == '"'  -> "\\\""
         | isPrint c -> singleton c
         | otherwise ->
           let
             h = hexadecimal (ord c)
             l = length h
           in if l <= 4
              then "\\u" <> fromString (replicate (4 - l) '0' <> h)
              else "\\U" <> fromString (replicate (8 - l) '0' <> h)

-}
