module Lang.Cue.Printer
  ( display
  , displayStrict
  , buildString
  ) where

import           Data.Char
import           Data.Functor           ((<&>))
import           Data.List              (intersperse)
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (catMaybes, maybeToList)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Lazy         (toStrict)
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder

import           Lang.Cue.Grammar


--------------------------------------------------------------------------------
-- API

class Printer a where
  build :: Int -> a -> Builder

display :: Printer a => a -> TL.Text
display = toLazyText . build 0

displayStrict :: Printer a => a -> T.Text
displayStrict = toStrict . display


--------------------------------------------------------------------------------
-- Base instances

instance Printer Builder where
  build _ = id

instance Printer Integer where
  build _ = fromString . show

instance Printer Double where
  build _ = fromString . show

instance Printer Bool where
  build _ False = "false"
  build _ True  = "true"


--------------------------------------------------------------------------------
-- Tokens

instance Printer Token where
  build i = \case
    TokenIdentifier    x -> build i x
    TokenKeyword       x -> build i x
    TokenOperator      x -> build i x
    TokenAttribute     x -> build i x
    TokenInterpolation x -> buildString i $ Left  x
    TokenString        x -> buildString i $ Right x
    TokenInteger       x -> build i x
    TokenFloat         x -> build i x

instance Printer [Token] where
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
  build i (Attribute name tokens) =
    "@" <> build i name <> "(" <> withSpaces i tokens <> ")"

instance Printer AttributeToken where
  build i = \case
    AttributeToken    t  -> build i t
    AttributeParens   ts -> "( " <> withSpaces i ts <> ")"
    AttributeBraces   ts -> "{ " <> withSpaces i ts <> "}"
    AttributeBrackets ts -> "[ " <> withSpaces i ts <> "]"

instance Printer InterpolationElement where
  build i = \case
    InterpolationString     t -> encodeText t
    InterpolationExpression e -> "\\(" <> build i e <> ")"


--------------------------------------------------------------------------------
-- AST

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
  build i = \case
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
    Unary          u       -> build i u
    Multiplication lhs rhs -> build i lhs <> " * "  <> build i rhs
    Division       lhs rhs -> build i lhs <> " / "  <> build i rhs
    Addition       lhs rhs -> build i lhs <> " + "  <> build i rhs
    Subtraction    lhs rhs -> build i lhs <> " - "  <> build i rhs
    Equal          lhs rhs -> build i lhs <> " == " <> build i rhs
    NotEqual       lhs rhs -> build i lhs <> " != " <> build i rhs
    Match          lhs rhs -> build i lhs <> " =~ " <> build i rhs
    NotMatch       lhs rhs -> build i lhs <> " !~ " <> build i rhs
    LessThan       lhs rhs -> build i lhs <> " < "  <> build i rhs
    LessOrEqual    lhs rhs -> build i lhs <> " <= " <> build i rhs
    GreaterThan    lhs rhs -> build i lhs <> " > "  <> build i rhs
    GreaterOrEqual lhs rhs -> build i lhs <> " >= " <> build i rhs
    LogicalAnd     lhs rhs -> build i lhs <> " && " <> build i rhs
    LogicalOr      lhs rhs -> build i lhs <> " || " <> build i rhs
    Unification    lhs rhs -> build i lhs <> " & "  <> build i rhs
    Disjunction    lhs rhs -> build i lhs <> " | "  <> build i rhs

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
      [ "[ " <> build (i+1) x <> ",\n" <> indent
      , mconcat $ r <&> \x -> "  " <> build (i+1) x <> ",\n" <> indent
      , "]"
      ]
    OpenList []    ell -> "[" <> build i ell <> "]"
    OpenList (x:r) ell -> mconcat
      [ "[ " <> build (i+1) x <> ",\n" <> indent
      , mconcat $ r <&> \x -> "  " <> build (i+1) x <> ",\n" <> indent
      , "  " <> build (i+1) ell <> ",\n" <> indent
      , "]"
      ]
    where
      indent = fromString $ replicate (2*i) ' '

--------------------------------------------------------------------------------
-- Helpers

makeBlock :: Printer a => Int -> [a] -> Builder
makeBlock i []  = "{}"
makeBlock i [x] = "{" <> build i x <> "}"
makeBlock i l   = mconcat
  [ "{\n"
  , mconcat $ l <&> \x -> indent <> "  " <> build (i+1) x <> ",\n"
  , indent
  , "}"
  ]
  where indent = fromString $ replicate (2*i) ' '

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
