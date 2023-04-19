module Lang.Cue.Builtins where

import "this" Prelude

import Data.HashMap.Strict qualified as M
import Data.List           qualified as L
import Data.Text           qualified as T

import Lang.Cue.Document   as D
import Lang.Cue.Error
import Lang.Cue.IR         as I


--------------------------------------------------------------------------------
-- * Builtin functions

len :: Function
len = Function "len" \args -> do
  validateArgsLength 1 args
  case args of
    -- can resolve
    [Atom (String s)] -> pure $ Atom $ Integer $ toInteger $ T.length s
    [Atom (Bytes  s)] -> pure $ Atom $ Integer $ toInteger $ T.length s
    [Struct s]        -> pure $ Atom $ Integer $ toInteger $ M.size $ structFields s
    [D.List l]        -> pure $ Atom $ Integer $ toInteger $ L.length l

    -- not evaluated enough, but potentially valid, bail out
    [Thunk       _]   -> Left ()
    [StringBound _]   -> Left ()
    [BytesBound  _]   -> Left ()
    [NotNull      ]   -> Left ()

    -- error
    [_wrong]          -> Left ()
    _                 -> unreachable

--------------------------------------------------------------------------------
-- * Helpers

validateArgsLength :: Int -> [Document] -> Either () ()
validateArgsLength n args =
  when (n /= length args) $
    Left ()
