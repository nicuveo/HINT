import           Data.Functor
import           Data.List.NonEmpty
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec    (errorBundlePretty, runParser, eof)

import           Lang.Cue.Parser


main = defaultMain allTests

allTests = testGroup "Tests" [runParserTests]

--------------------------------------------------------------------------------
-- Parser

runParserTests = testGroup "Parser"
  [ testGroup "expression" $ expressionTests <&> \(n, i, o) ->
      testCase n $ testParser expression i o
  , testGroup "sourceFile" $ sourceFileTests <&> \(n, i, o) ->
      testCase n $ testParser sourceFile i o
  , testGroup "number literal" $ numberLitTests <&> \(n, i, o) ->
      testCase n $ testParser numberLiteral i o
  ]

testParser parser input ast = case runParser (parser <* eof) "<interactive>" input of
  Right result -> result @?= ast
  Left  err    -> assertFailure $ errorBundlePretty err


expressionTests =
  [ ( "bounds"
    , "2 & >=2 & <=5 | 2.5 & >=1 & <=5 | 2 & >=1.0 & <3.0 | 2 & >1 & <3.0 | 2.5 & int & >1 & <5 | 2.5 & float & >1 & <5 | int & 2 & >1.0 & <3.0 | 2.5 & >=(int & 1) & <5 | >=0 & <=7 & >=3 & <=10 | !=null & 1 | >=5 & <=5"
    , Unary $ PrimaryExpression $ PrimaryOperand (OperandName (QualifiedIdentifier {qiPackageName = Nothing, qiIdentifier = Identifier "foo"}))
    )
  ]

numberLitTests =
  [ ( "decimal 0"
    , "0"
    , Right 0
    )
  , ( "decimal"
    , "79_537_537"
    , Right 79537537
    )
  , ( "oct"
    , "0o4563"
    , Right 0o4563
    )
  , ( "si 1"
    , "1234.56Ki"
    , Right 1264189
    )
  ]

sourceFileTests =
  [ ( "package name"
    , "package foo"
    , SourceFile (Just $ Identifier "foo") [] [] []
    )
  , ( "top level attribute"
    , "@z(id = let)"
    , SourceFile Nothing [Attribute "z" $ AttributeToken <$> [TokenIdentifier "id", TokenOperator OperatorBind, TokenKeyword KeywordLet]] [] []
    )
  -- , ( "import statement"
  --   , "import \"blaaah\""
  --   , SourceFile Nothing [] [Import Nothing "blaah" Nothing] []
  --   )
  , ( "all combined"
    , "@package(attr=fixme),package floof\na: b: foo\n"
    , SourceFile
      { moduleName = Just (Identifier "floof")
      , moduleAttributes = [Attribute {attributeName = Identifier "package", attributeTokens = [AttributeToken (TokenIdentifier (Identifier "attr")), AttributeToken (TokenOperator OperatorBind),AttributeToken (TokenIdentifier (Identifier "fixme"))]}]
      , moduleImports = []
      , moduleDeclarations =
        [ DeclarationField (Field {fieldLabels = Label {labelIdentifier = Nothing, labelExpression = LabelName Required "a"} :| [Label {labelIdentifier = Nothing, labelExpression = LabelName Required "b"}], fieldExpression = AliasedExpression {aeAlias = Nothing, aeExpression = Unary (PrimaryExpression (PrimaryOperand (OperandName (QualifiedIdentifier {qiPackageName = Nothing, qiIdentifier = Identifier "foo"}))))}, fieldAttributes = []})
        ]
      }
    )
  ]
