module Lang.Cue.Translate
  ( translateFile
  , translateExpression
  ) where

import "this" Prelude

import Control.Monad.Validate
import Data.HashMap.Strict    qualified as M

import Lang.Cue.AST           as A
import Lang.Cue.Document      as D
import Lang.Cue.Error
import Lang.Cue.Token


--------------------------------------------------------------------------------
-- * API

translateFile :: SourceFile -> Either [Error] Block
translateFile SourceFile {..} = runValidate do
  b <- translateBlock moduleDeclarations
  pure b { canBeAtom = True }

translateExpression :: Expression -> Either [Error] Thunk
translateExpression = runValidate . translateExpr


--------------------------------------------------------------------------------
-- * Translation

type Translation f t = f -> Validate [Error] t

translateBlock :: Translation [Declaration] Block
translateBlock = foldM translateDeclaration $ Definitions
  { defFields      = M.empty
  , defConstraints = []
  , defAliases     = M.empty
  , defAttributes  = M.empty
  , defCanBeAtom   = False
  }

{-
translateDeclaration :: Block -> Translation Declaration Block
translateDeclaration block = \case
  DeclarationAttribute Attribute {..} ->
    pure block
      { defAttributes = M.insertWith (++) defAttributes block attributeName [attributeText]
      }
  DeclarationLetClause LetClause {..} -> do
    pure block
-}
