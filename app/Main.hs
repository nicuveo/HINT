import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Loops
import           System.IO

import           Lang.Cue.Eval


main :: IO ()
main = whileJust_ readPrompt evalLine

readPrompt :: IO (Maybe String)
readPrompt = do
  putStr "λ "
  hFlush stdout
  line <- getLine
  pure $
    if | line == ":q" -> Nothing
       | otherwise    -> Just line

evalLine :: String -> IO ()
evalLine l = do
  putStrLn $ "I dunno lol"
