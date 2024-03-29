module Lang.Cue.Packages.Builtins
  ( lenFunc
  , closeFunc
  , andFunc
  , orFunc
  , divFunc
  , modFunc
  , quotFunc
  , remFunc
  ) where

import "this" Prelude

import Data.HashMap.Strict           qualified as M
import Data.List                     qualified as L
import Data.Sequence
import Data.Text                     qualified as T

import Lang.Cue.Representation.IR    as I
import Lang.Cue.Representation.Value as V
import Lang.Cue.Stages.Eval


--------------------------------------------------------------------------------
-- * Builtin functions

lenFunc :: Function
lenFunc = Function "len" \args -> do
  validateArgsLength 1 args
  case args of
    [Atom (String s)] -> pure $ Atom $ Integer $ toInteger $ T.length s
    [Atom (Bytes  s)] -> pure $ Atom $ Integer $ toInteger $ T.length s
    [Struct s]        -> pure $ Atom $ Integer $ toInteger $ M.size   $ _sFields s
    [V.List l]        -> pure $ Atom $ Integer $ toInteger $ L.length $ _lValues l
    _                 -> report undefined

closeFunc :: Function
closeFunc = Function "close" \args -> do
  validateArgsLength 1 args
  case args of
    [Struct s] -> pure $ Struct s { _sClosed = True }
    _          -> report undefined

andFunc :: Function
andFunc = Function "and" $ foldlM unify V.Top

orFunc :: Function
orFunc = Function "or" \args -> do
  when (L.null args) $ report undefined
  pure $ Disjoint (fromList args) Empty

divFunc, modFunc, quotFunc, remFunc :: Function
divFunc  = division "div"  div
modFunc  = division "mod"  mod
quotFunc = division "quot" quot
remFunc  = division "rem"  rem


--------------------------------------------------------------------------------
-- * Helpers

validateArgsLength :: Int -> [Value] -> Eval ()
validateArgsLength n args =
  when (n /= L.length args) $
    report undefined

division :: Text -> (Integer -> Integer -> Integer) -> Function
division name (/%) = Function name \case
  [n, d] -> do
    ni <- case n of { Atom (Integer i) -> pure i; _ -> report undefined }
    di <- case d of { Atom (Integer i) -> pure i; _ -> report undefined }
    when (di == 0) $ report undefined
    pure $ Atom $ Integer $ ni /% di
  _ -> report undefined
