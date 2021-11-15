module Lang.Cue.Printer where

import Data.Functor ((<&>))
import Data.Maybe (maybeToList, catMaybes)
import Data.List (intersperse)
import Data.Text.Lazy.Builder (Builder, fromText, fromString, toLazyText)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Lang.Cue.Parser


-- class

class Printer a where
  build :: Int -> a -> Builder

display :: Printer a => a -> TL.Text
display = toLazyText . build 0

displayStrict :: Printer a => a -> T.Text
displayStrict = toStrict . display


-- base instances

instance Printer Builder where
  build _ = id

instance Printer T.Text where
  build _ _ = "<escaped text>"

instance Printer Integer where
  build _ = fromString . show

instance Printer Double where
  build _ = fromString . show

instance Printer Bool where
  build _ False = "false"
  build _ True  = "true"


-- tokens

instance Printer Token where
  build i = \case
    TokenIdentifier    x -> build i x
    TokenKeyword       x -> build i x
    TokenOperator      x -> build i x
    TokenAttribute     x -> build i x
    TokenInterpolation _ -> "<interpolation>"
    TokenString        x -> "\"" <> build i x <> "\""
    TokenInteger       x -> build i x
    TokenFloat         x -> build i x

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
    "@" <> build i name <> "(" <> listWith i " " tokens <> ")"

instance Printer AttributeToken where
  build i = \case
    AttributeToken    t  -> build i t
    AttributeParens   ts -> "(" <> listWith i " " ts <> ")"
    AttributeBraces   ts -> "{" <> listWith i " " ts <> "}"
    AttributeBrackets ts -> "[" <> listWith i " " ts <> "]"


-- ast

instance Printer SourceFile where
  build i (SourceFile ident attribs imports decls) =
    listWith i "\n" $ concat
      [ maybeToList $ ident <&> \n -> "package " <> build i n
      , build i <$> attribs
      , build i <$> imports
      , build i <$> decls
      , [""] -- forces a newline at the end of the file
      ]

instance Printer Import where
  build i (Import name path ident) =
    mconcat $ intersperse " " $ catMaybes
      [ Just "import"
      , build i <$> name
      , Just $ "\"" <> fromText path <> "\""
      , build i <$> ident
      ]

instance Printer Declaration where
  build i = \case
    DeclarationField     x -> build i x
    DeclarationEllipsis  x -> build i x
    DeclarationEmbedding x -> build i x
    DeclarationLetClause x -> build i x
    DeclarationAttribute x -> build i x

instance Printer Field where
  build _ _ = "<field>"

instance Printer Label where
  build _ _ = "<label>"

instance Printer LabelExpression where
  build i = \case
    LabelName  opt name -> build i name <> build i opt
    LabelAlias alexp    -> "[" <> build i alexp <> "]"

instance Printer Optional where
  build i = \case
    Optional -> "?"
    Required -> ""

instance Printer Ellipsis where
  build i e = "..." <> maybe "" (\x -> " " <> build i x) e

instance Printer Embedding where
  build i = \case
    EmbeddedComprehension _ -> "<comprehension>"
    EmbeddedExpression    a -> build i a

instance Printer LetClause where
  build i (LetClause ident expr) =
    mconcat $ intersperse " "
      [ "let"
      , build i ident
      , "="
      , build i expr
      ]

instance Printer AliasedExpression where
  build i (AliasedExpression alias expr) =
    maybe "" (\a -> build i a <> " = ") alias <> build i expr

instance Printer Expression where
  build i = \case
    Unary          u       -> build i u
    Multiplication lhs rhs -> "(" <> build i lhs <> "*"  <> build i rhs <> ")"
    Division       lhs rhs -> "(" <> build i lhs <> "/"  <> build i rhs <> ")"
    Addition       lhs rhs -> "(" <> build i lhs <> "+"  <> build i rhs <> ")"
    Subtraction    lhs rhs -> "(" <> build i lhs <> "-"  <> build i rhs <> ")"
    Equal          lhs rhs -> "(" <> build i lhs <> "==" <> build i rhs <> ")"
    NotEqual       lhs rhs -> "(" <> build i lhs <> "!=" <> build i rhs <> ")"
    Match          lhs rhs -> "(" <> build i lhs <> "=~" <> build i rhs <> ")"
    NotMatch       lhs rhs -> "(" <> build i lhs <> "!~" <> build i rhs <> ")"
    LessThan       lhs rhs -> "(" <> build i lhs <> "<"  <> build i rhs <> ")"
    LessOrEqual    lhs rhs -> "(" <> build i lhs <> "<=" <> build i rhs <> ")"
    GreaterThan    lhs rhs -> "(" <> build i lhs <> ">"  <> build i rhs <> ")"
    GreaterOrEqual lhs rhs -> "(" <> build i lhs <> ">=" <> build i rhs <> ")"
    LogicalAnd     lhs rhs -> "(" <> build i lhs <> "&&" <> build i rhs <> ")"
    LogicalOr      lhs rhs -> "(" <> build i lhs <> "||" <> build i rhs <> ")"
    Unification    lhs rhs -> "(" <> build i lhs <> "&"  <> build i rhs <> ")"
    Disjunction    lhs rhs -> "(" <> build i lhs <> "|"  <> build i rhs <> ")"

instance Printer UnaryExpression where
  build i = \case
    UnaryExpression o u -> build i o <> build i u
    PrimaryExpression p -> build i p

instance Printer PrimaryExpression where
  build i = \case
    PrimaryOperand  o       -> build i o
    PrimarySelector e s     -> build i e <> "." <> build i s
    PrimaryIndex    e j     -> build i e <> "[" <> build i j <> "]"
    PrimarySlice    e (l,h) -> build i e <> "[" <> build i l <> ":" <> build i h <> "]"
    PrimaryCall     e args  -> build i e <> "(" <> listWith i ", " args <> ")"

instance Printer Operand where
  build i = \case
    OperandLiteral    l -> build i l
    OperandName       n -> build i n
    OperandExpression e -> build i e

instance Printer QualifiedIdentifier where
  build i (QualifiedIdentifier pn ident) =
    maybe "" (\n -> build i n <> ".") pn <> build i ident

instance Printer Literal where
  build i = \case
    IntegerLiteral n -> build i n
    FloatLiteral d   -> build i d
    StringLiteral t  -> "\"" <> build i t <> "\""
    BoolLiteral b    -> build i b
    NullLiteral      -> "null"
    BottomLiteral    -> "_|_"
    ListLiteral l    -> build i l
    StructLiteral ds -> blockWith i "{" "}" "" ds

instance Printer ListLiteral where
  build i = \case
    ClosedList ems      -> blockWith i "[" "]" "," ems
    OpenList   ems ells -> blockWith i "[" (build i ells <> "]") "," ems


-- helpers

blockWith :: Printer a => Int -> Builder -> Builder -> Builder -> [a] -> Builder
blockWith i open close sep elems = mconcat
  [ open
  , "\n  "
  , indent
  , listWith (i+1) (sep <> "\n  " <> indent) elems
  , "\n"
  , indent
  , close
  ]
  where indent = fromString $ replicate (2*i) ' '

listWith :: Printer a => Int -> Builder -> [a] -> Builder
listWith i s = mconcat . intersperse s . map (build i)
