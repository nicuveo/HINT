module Lang.Cue.Error where

import "this" Prelude

import Data.Char
import Data.Text         qualified as T
import GHC.Stack

import Lang.Cue.Location


--------------------------------------------------------------------------------
-- * Panic

data Panic
  = UnreachableCode
  | PopEmptyPath
  | ThunkNotFound

unreachable :: HasCallStack => a
unreachable = withFrozenCallStack $ panic UnreachableCode

panic :: HasCallStack => Panic -> a
panic p = withFrozenCallStack $ error $ pp $ case p of
  UnreachableCode -> "001: reached supposedly unreachable code"
  PopEmptyPath    -> "002: attempted to pop an already empty path"
  ThunkNotFound   -> "003: failed to retrieve a known thunk"
  where
    pp m = "PANIC\n\
           \hint as encountered an unrecoverable internal error.\n\
           \Please report this error with the following error code and call stack:\n\
           \ErrorCode: " ++ m


--------------------------------------------------------------------------------
-- * User error

data ErrorInfo
  = LexerTokenError (Maybe (Int, String)) [String]
  | LexerCustomError String
  | ParserError
  | BottomError BottomSource
  deriving (Show)

data BottomSource
  = ArisedFromLiteral
  deriving (Show)

type Error  = WithLocation ErrorInfo
type Errors = Seq Error

errorMessage :: Error -> Text
errorMessage (WithLocation (loc@Location {..}, e)) = case e of
  LexerTokenError unexpected expected -> case unexpected of
    Nothing -> msg 1
      [ "lexer error: unexpected token"
      , "expecting one of: " <> T.intercalate ", " (T.pack . show <$> expected)
      ]
    Just (l, u) -> msg l
      [ "lexer error: unexpected token"
      , "expecting one of: " <> T.intercalate ", " (map T.pack expected)
      , "but instead found: " <> T.pack u
      ]
  LexerCustomError err -> msg 1 ["lexer error", T.pack err]
  ParserError -> msg 1 ["parse error"]
  BottomError b -> T.pack $ "_|_: " ++ show b -- FIXME
  where
    (row, col) = findPos loc
    currentLine = locCode !! (row-1)
    (T.length -> removed, code) = T.span isSpace currentLine
    msg s m = T.unlines $ headerLine : map ("    " <>) m <> ["", codeLine, underLine s]
    headerLine = T.pack $ locFilename <> ":" <> show row <> ":" <> show col <> ": error:"
    codeLine    = "          " <> code
    underLine s = "          " <> T.replicate (col - removed) " " <> T.replicate s "^"

findPos :: Location -> (Int, Int)
findPos (Location {..}) = go 1 locOffset locCode
  where
    go !r !o = \case
      []     -> unreachable
      (l:ls) -> let n = T.length l + 1 in
        if o > n
        then go (r + 1) (o - n) ls
        else (r, o)
