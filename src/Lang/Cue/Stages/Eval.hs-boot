module Lang.Cue.Stages.Eval where

import "this" Prelude

import Lang.Cue.Error
import {-# SOURCE #-} Lang.Cue.Representation.IR


newtype Eval a = Eval
  { runEval :: ReaderT Scope (StateT [Path] (Except EvalError)) a
  }

data Scope
data EvalError

report :: BottomSource -> Eval a
