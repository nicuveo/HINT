module Lang.Cue.Error where

import "this" Prelude

import Lang.Cue.Location


--------------------------------------------------------------------------------
-- * Panic

data Panic
  = UnreachableCode
  | DemoteBaseValue

unreachable :: a
unreachable = panic UnreachableCode

panic :: Panic -> a
panic = error . pp . \case
  UnreachableCode -> "01: reached supposedly unreachable code"
  DemoteBaseValue -> "02: attempted to demote an already-demoted value"
  where
    pp m = "PANIC\n\
           \hint as encountered an unrecoverable internal error.\n\
           \Please report this error with the following error code and call stack:\n\
           \ErrorCode: " ++ m


--------------------------------------------------------------------------------
-- * Parsing

data ParseErrorCode
  = LexerError

type ParseError = WithLocation ParseErrorCode
