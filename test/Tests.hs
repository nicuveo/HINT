module Main where

import Control.Monad.IO.Class
import Data.Functor
import Data.List.NonEmpty
import Data.Text              qualified as T
import Data.Text.IO           qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec        hiding (Label, token)

import Lang.Cue.Grammar
import Lang.Cue.Parser
import Lang.Cue.Printer

import Arbitrary


--------------------------------------------------------------------------------
-- Main

main = defaultMain $ testGroup "Tests"
  [ testGroup "Unit tests" [parserUTests, printerUTests]
  , testGroup "E2E tests"  [parserETests]
  ]


--------------------------------------------------------------------------------
-- Parser unit tests

parserUTests = testGroup "Parser"
  [ testGroup "expression" $ expressionTests <&> \(n, i, o) ->
      testCase n $ testParser expression i o
  , testGroup "sourceFile" $ sourceFileTests <&> \(n, i, o) ->
      testCase n $ testParser sourceFile i o
  , testGroup "number literal" $ numberLitTests <&> \(n, i, o) ->
      testCase n $ testParser numberLiteral i o
  , testGroup "edge cases" edgeCasesTests
  ]

testParser parser input ast = case runParser (parser <* eof) "<interactive>" input of
  Right result -> result @?= ast
  Left  err    -> assertFailure $ errorBundlePretty err

expressionTests =
  [ ( "bounds"
    , ">= 5.0"
    , Unary (UnaryExpression [OperatorGTE] (PrimaryOperand (OperandLiteral (FloatLiteral 5))))
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
    , SourceFile (Just "foo") [] [] []
    )
  , ( "top level attribute"
    , "@z(id = let)"
    , SourceFile Nothing [Attribute "z" $ AttributeToken <$> [TokenIdentifier "id", TokenOperator OperatorBind, TokenKeyword KeywordLet]] [] []
    )
  , ( "import statement"
    , "import \"blaaah\""
    , SourceFile Nothing [] [Import Nothing "blaaah" Nothing] []
    )
  ]

edgeCasesTests =
  [ testCase "bottom is not a disjunction of identifiers" $ testParser expression "_|_" $
    Unary $ UnaryExpression [] $ PrimaryOperand $ OperandLiteral $ BottomLiteral
  , testCase "#foo is an identifier and not an invalid string literal" $ testParser expression "#foo" $
    Unary $ UnaryExpression [] $ PrimaryOperand $ OperandName $ QualifiedIdentifier Nothing "#foo"
  , testCase "foo.bar is ambiguous but interpreted as a qualified identifier" $ testParser expression "foo.bar" $
    Unary $ UnaryExpression [] $ PrimaryOperand $ OperandName $ QualifiedIdentifier (Just "foo") "bar"
  , testCase "(foo).bar is not ambiguous" $ testParser expression "(foo).bar" $
    Unary $ UnaryExpression [] $ PrimarySelector (PrimaryOperand $ OperandExpression $ Unary $ UnaryExpression [] $ PrimaryOperand $ OperandName $ QualifiedIdentifier Nothing "foo") $ Left "bar"
  ]

--------------------------------------------------------------------------------
-- Printer unit tests

printerUTests = testGroup "Parser"
  [ testRoundTrips
  ]

testRoundTrips = testGroup "round trip"
  [ roundTrip "token"       (token `manyTill` eof)
  , roundTrip "literal"     literal
  , roundTrip "expression"  expression
  , roundTrip "source file" sourceFile
  ]

roundTrip n p = testProperty n \x ->
  runParser (p <* eof) "" (display x) == Right x


--------------------------------------------------------------------------------
-- Parser E2E tests

parserETests = testGroup "Parser"
  [ testCase "basic" $ checkFile "basic"
  ]

checkFile name = do
  let
    fileNameIn  = "test/data/" <> name <> ".in"
    fileNameOut = "test/data/" <> name <> ".out"
  inFile  <- liftIO $ T.readFile fileNameIn
  outFile <- liftIO $ T.readFile fileNameOut
  ast <- case runParser sourceFile fileNameIn inFile of
    Right result -> pure result
    Left  err    -> assertFailure $ errorBundlePretty err
  outFile @=? display ast
