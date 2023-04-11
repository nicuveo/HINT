module Lang.Cue.Error where

import "this" Prelude

import Lang.Cue.Location


--------------------------------------------------------------------------------
-- * Panic

data Panic
  = UnreachableCode
  | DemoteBaseValue
  | AmbiguousParse

unreachable :: a
unreachable = panic UnreachableCode

panic :: Panic -> a
panic = error . pp . \case
  UnreachableCode -> "001: reached supposedly unreachable code"
  DemoteBaseValue -> "002: attempted to demote an already-demoted value"
  AmbiguousParse  -> "003: parsing was ambiguous and returned more than one result"
  where
    pp m = "PANIC\n\
           \hint as encountered an unrecoverable internal error.\n\
           \Please report this error with the following error code and call stack:\n\
           \ErrorCode: " ++ m


--------------------------------------------------------------------------------
-- * Parsing

data ErrorCode
  = LexerError

type Error = WithLocation ErrorCode

errorMessage :: Error -> Text
errorMessage _ = "lexer failed"
