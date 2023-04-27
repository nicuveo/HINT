module Lang.Cue.Eval where

import "this" Prelude

import Lang.Cue.Error


newtype Eval a = Eval
  { runEval :: ReaderT Scope (Except EvalError) a
  }

data Scope
data EvalError

report :: BottomSource -> Eval a
