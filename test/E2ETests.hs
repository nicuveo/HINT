import qualified Data.Text.IO as T
import           Data.Functor
import           Data.List.NonEmpty
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Megaparsec    (errorBundlePretty, runParser, eof)
import Control.Monad.IO.Class
import System.Directory

import           Lang.Cue.Parser
import           Lang.Cue.Printer


main = defaultMain allTests

allTests = testGroup "Tests" [runParserTests]

--------------------------------------------------------------------------------
-- Parser

runParserTests = testGroup "Parser"
  [ testCase "basic" $ testParser "basic"
  ]

testParser name = do
  let
    fileNameIn  = "test/queries/" <> name <> ".in"
    fileNameOut = "test/queries/" <> name <> ".out"
  inFile  <- liftIO $ T.readFile fileNameIn
  outFile <- liftIO $ T.readFile fileNameOut
  ast <- case runParser sourceFile fileNameIn inFile of
    Right result -> pure result
    Left  err    -> assertFailure $ errorBundlePretty err
  outFile @=? displayStrict ast
