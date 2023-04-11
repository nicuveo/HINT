module Main where

import "this" Prelude

import Control.Monad.Loops
import Control.Monad.Trans      (lift)
import Data.Char
import Data.Text                qualified as T
import Data.Text.IO             qualified as T
import System.Console.Haskeline
import System.Exit

import Lang.Cue.Error
import Lang.Cue.Eval
import Lang.Cue.HKD
import Lang.Cue.Lexer
import Lang.Cue.Location
import Lang.Cue.Parser


main :: IO ()
main = runInputT defaultSettings $ whileJust_ readPrompt (lift . evalLine)

readPrompt :: InputT IO (Maybe String)
readPrompt = do
  line <- getInputLine "cue> "
  pure $ line >>= \l ->
    if | l == ":q" -> Nothing
       | otherwise -> Just l

evalLine :: String -> IO ()
evalLine (dropWhile isSpace -> l) = case l of
  [] -> pure ()
  (':' : c) -> do
    let (cmd, dropWhile isSpace -> expr) = break isSpace c
    case cmd of
      "?"   -> usage
      "q"   -> exitSuccess
      "tok" -> display $ fmap (map $ reify discardLocation) $ tokenize "<interactive>" $ T.pack expr
      "ast" -> display $ parse expression "<interactive>" $ T.pack expr
      _     -> usage
  _ -> display $ fmap eval $ parse expression "<interactive>" $ T.pack l
  where
    display :: Show a => Either [Error] a -> IO ()
    display = either (T.putStr . T.unlines . intersperse "" . map errorMessage) print
    usage = putStrLn "\
      \:?     print this help\n\
      \:q     quit this REPL\n\
      \:tok   tokenize the given expression \n\
      \:ast   parse and print the AST of the given expression"

{-
prettyPrint :: Expression -> IO ()
prettyPrint = go 0
  where
    indent :: Int -> IO ()
    indent n = putStr $ replicate (2*n) ' '
    go :: Int -> Expression -> IO ()
    go n e = indent n >> pE n e
    pE :: Int -> Expression -> IO ()
    pE n = \case
      Unary          u     -> pU n u
      Multiplication a b c -> pB n "*"  (a:b:c)
      Division       a b c -> pB n "/"  (a:b:c)
      Addition       a b c -> pB n "+"  (a:b:c)
      Subtraction    a b c -> pB n "-"  (a:b:c)
      Equal          a b c -> pB n "==" (a:b:c)
      NotEqual       a b c -> pB n "!=" (a:b:c)
      Match          a b c -> pB n "=~" (a:b:c)
      NotMatch       a b c -> pB n "!~" (a:b:c)
      LessThan       a b c -> pB n "<"  (a:b:c)
      LessOrEqual    a b c -> pB n "<=" (a:b:c)
      GreaterThan    a b c -> pB n ">"  (a:b:c)
      GreaterOrEqual a b c -> pB n ">=" (a:b:c)
      LogicalAnd     a b c -> pB n "&&" (a:b:c)
      LogicalOr      a b c -> pB n "||" (a:b:c)
      Unification    a b c -> pB n "&"  (a:b:c)
      Disjunction    a b c -> pB n "|"  (a:b:c)
    pB :: Int -> String -> [Expression] -> IO ()
    pB n o es = do
      putStrLn o
      for_ es $ go (n + 1)
    pU :: Int -> UnaryExpression -> IO ()
    pU n (UnaryExpression o e) = putStr (show o ++ " ") >> pP n e
    pP :: Int -> PrimaryExpression -> IO ()
    pP n = \case
      PrimaryOperand o    -> pO n o
      PrimarySelector e i -> do
        putStrLn "selection"
        indent (n + 1) >> putStr "expr: " >> pP (n + 1) e
        indent (n + 1) >> putStr "seln: " >> putStrLn (show i)
      _ -> putStrLn "TODO"
    pO :: Int -> Operand -> IO ()
    pO n = \case
      OperandLiteral    l -> print l
      OperandName       i -> print i
      OperandExpression e -> pE n e
-}
