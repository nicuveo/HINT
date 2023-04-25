module Lang.Cue.Eval where

import                "this" Prelude

import                Control.Monad.ST
import                Data.STRef

import                Lang.Cue.Error
import {-# SOURCE #-} Lang.Cue.IR
import {-# SOURCE #-} Lang.Cue.Value


data ThunkCacheNode s = ThunkCacheNode
  { nodeThunk  :: Thunk
  , evalStatus :: Bool
  , subPaths   :: HashMap PathElem (STRef s (ThunkCacheNode s))
  }

type ThunkPtr s = STRef s (ThunkCacheNode s)

data EvalContext s = EvalContext
  { cacheRoot   :: STRef s (ThunkCacheNode s)
  , currentEval :: [Path]
  }

data EvalError
  = CannotBeEvaluatedYet
  | EvaluationFailed (Seq BottomSource)

newtype Eval s a = Eval
  { runEval :: ExceptT EvalError (StateT (EvalContext s) (ST s)) a
  }

report :: BottomSource -> Eval s a
typeMismatch :: String -> Value -> Eval s a
typeMismatch2  :: String -> Value -> Value -> Eval s a
