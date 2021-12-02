import           Control.Applicative       ((<$>))
import           Control.Arrow             (left)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe
import           Data.Char
import           Data.Foldable
import qualified Data.Text                 as T
import           System.Console.Haskeline
import           System.IO
import           Text.Megaparsec           hiding (token)
import           Text.Megaparsec.Error

import           Lang.Cue.Eval
import           Lang.Cue.Grammar
import           Lang.Cue.Parser


main :: IO ()
main = runInputT defaultSettings $ whileJust_ readPrompt (lift . evalLine)

readPrompt :: InputT IO (Maybe String)
readPrompt = do
  line <- getInputLine "λ "
  pure $ line >>= \l ->
    if | l == ":q" -> Nothing
       | otherwise -> Just l

evalLine :: String -> IO ()
evalLine l
  | all isSpace l = pure ()
  | otherwise     = void $ runMaybeT $ do
      liftIO $ putStrLn "# 1. tokens"
      case run tokenize l of
        Left e       -> liftIO (putStr e) >> empty
        Right tokens -> liftIO $ print tokens
      liftIO $ putStrLn "# 2. ast"
      e <- case run expression l of
        Left e  -> liftIO (putStr e) >> empty
        Right a -> liftIO (prettyPrint a) >> pure a
      liftIO $ putStrLn "# 3. eval"
      liftIO $ print $ eval e


run :: Parser a -> String -> Either String a
run p i = left errorBundlePretty $ runParser (p <* eof) "<interactive>" (T.pack i)

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
      for_ es $ go (n+1)
    pU :: Int -> UnaryExpression -> IO ()
    pU n (UnaryExpression o e) = putStr (show o ++ " ") >> pP n e
    pP :: Int -> PrimaryExpression -> IO ()
    pP n = \case
      PrimaryOperand o    -> pO n o
      PrimarySelector e i -> do
        putStrLn "selection"
        indent (n+1) >> putStr "expr: " >> pP (n+1) e
        indent (n+1) >> putStr "seln: " >> putStrLn (show i)
      _ -> putStrLn "TODO"
    pO :: Int -> Operand -> IO ()
    pO n = \case
      OperandLiteral    l -> print l
      OperandName       i -> print i
      OperandExpression e -> pE n e
