module Main where

import "this" Prelude

import Data.Text        qualified as T
-- import Data.Text.IO           qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Lang.Cue.AST
import Lang.Cue.Error
import Lang.Cue.Parser
import Lang.Cue.Tokens

-- import Arbitrary


--------------------------------------------------------------------------------
-- Main

main = defaultMain $ testGroup "Tests"
  [ testGroup "Unit tests" [parserUTests] --, printerUTests]
--  , testGroup "E2E tests"  [parserETests]
  ]


--------------------------------------------------------------------------------
-- Parser unit tests

parserUTests = testGroup "Parser"
  [ testGroup "expression" $ expressionTests <&> \(n, i, o) ->
      testCase n $ testParser expression i o
  , testGroup "sourceFile" $ sourceFileTests <&> \(n, i, o) ->
      testCase n $ testParser sourceFile i o
  , testGroup "number literal" $ numberLitTests <&> \(n, i, o) ->
      testCase n $ testParser expression i o
  , testGroup "edge cases" edgeCasesTests
  ]

testParser :: (Show a, Eq a) => (forall r. Grammar r a) -> Text -> a -> Assertion
testParser parser input ast = case parse parser "<interactive>" input of
  Right result -> result @?= ast
  Left  err    -> assertFailure $ T.unpack $ foldMap errorMessage err

expressionTests =
  [ ( "bounds"
    , ">= 5.0"
    , Unary (UnaryExpression [OperatorGTE] (PrimaryOperand (OperandLiteral (FloatLiteral 5))))
    )
  ]

numberLitTests =
  [ ( "decimal 0"
    , "0"
    , Unary $ UnaryExpression [] $ PrimaryOperand $ OperandLiteral $ IntegerLiteral 0
    )
  , ( "decimal"
    , "79_537_537"
    , Unary $ UnaryExpression [] $ PrimaryOperand $ OperandLiteral $ IntegerLiteral 79537537
    )
  , ( "oct"
    , "0o4563"
    , Unary $ UnaryExpression [] $ PrimaryOperand $ OperandLiteral $ IntegerLiteral 0o4563
    )
  , ( "si 1"
    , "1234.56Ki"
    , Unary $ UnaryExpression [] $ PrimaryOperand $ OperandLiteral $ IntegerLiteral 1264189
    )
  ]

sourceFileTests =
  [ ( "package name"
    , "package foo"
    , SourceFile (Just "foo") [] [] []
    )
  , ( "top level attribute"
    , "@z(id = let)"
    , SourceFile Nothing [Attribute "z" "id = let"] [] []
    )
  , ( "import statement"
    , "import \"blaaah\""
    , SourceFile Nothing [] [Import Nothing "blaaah"] []
    )
  ]

edgeCasesTests =
  [ testCase "bottom is not a disjunction of identifiers" $ testParser expression "_|_" $
    Unary $ UnaryExpression [] $ PrimaryOperand $ OperandLiteral $ BottomLiteral
  , testCase "#foo is an identifier and not an invalid string literal" $ testParser expression "#foo" $
    Unary $ UnaryExpression [] $ PrimaryOperand $ OperandName $ Identifier "#foo"
  , testCase "foo.bar is technically ambiguous but interpreted as a selector" $ testParser expression "foo.bar" $
    Unary $ UnaryExpression [] $ PrimarySelector (PrimaryOperand $ OperandName $ Identifier "foo") $ Left "bar"
  , testCase "(foo).bar is not ambiguous" $ testParser expression "(foo).bar" $
    Unary $ UnaryExpression [] $ PrimarySelector (PrimaryOperand $ OperandExpression $ Unary $ UnaryExpression [] $ PrimaryOperand $ OperandName $ Identifier "foo") $ Left "bar"
  ]

--------------------------------------------------------------------------------
-- Printer unit tests

{-

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

-}

--------------------------------------------------------------------------------
-- Parser E2E tests

{-
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
-}
