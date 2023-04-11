module Lang.Cue.NoShow where

import "this" Prelude

newtype NoShow = NoShow String
  deriving IsString

instance Show NoShow where
  show (NoShow s) = s
